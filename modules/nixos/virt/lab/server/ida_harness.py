# IDAPython harness, run INSIDE THE GUEST by idat in batch mode:
#   idat.exe -A -L<log> -S"C:\analysis\ida\ida_harness.py <job.json>" <sample>
# Reads a job description, performs one action, writes JSON to job["output"],
# then exits IDA. Targets IDA 9.x (IDAPython 3); most of it also works on 8.x.

import json
import sys
import traceback

import ida_auto
import ida_bytes
import ida_entry
import ida_funcs
import ida_hexrays
import ida_ida
import ida_kernwin
import ida_lines
import ida_nalt
import ida_name
import ida_segment
import idaapi
import idautils
import idc

MAX_STR = 400


def resolve(target):
    t = str(target).strip()
    if t.lower().startswith("0x"):
        return int(t, 16)
    ea = ida_name.get_name_ea(idaapi.BADADDR, t)
    if ea != idaapi.BADADDR:
        return ea
    if t and all(c in "0123456789abcdefABCDEF" for c in t):
        return int(t, 16)
    return None


def resolve_any(target):
    ea = resolve(target)
    if ea is None:
        # Imports are often named __imp_X or X_0; try a few spellings.
        for alt in ("__imp_" + str(target), "__imp__" + str(target), str(target) + "_0"):
            ea = ida_name.get_name_ea(idaapi.BADADDR, alt)
            if ea != idaapi.BADADDR:
                return ea
    return ea


def overview(job):
    info = {
        "input_file": ida_nalt.get_root_filename(),
        "input_sha256": ida_nalt.retrieve_input_file_sha256().hex() if ida_nalt.retrieve_input_file_sha256() else None,
        "processor": ida_ida.inf_get_procname(),
        "bitness": 64 if ida_ida.inf_is_64bit() else 32,
        "image_base": hex(idaapi.get_imagebase()),
    }
    segs = []
    for s in idautils.Segments():
        seg = ida_segment.getseg(s)
        segs.append({
            "name": ida_segment.get_segm_name(seg),
            "start": hex(seg.start_ea), "end": hex(seg.end_ea),
            "perm": "".join(c if seg.perm & b else "-" for c, b in (("r", 4), ("w", 2), ("x", 1))),
        })
    entries = []
    for i in range(ida_entry.get_entry_qty()):
        ordn = ida_entry.get_entry_ordinal(i)
        entries.append({"name": ida_entry.get_entry_name(ordn), "ea": hex(ida_entry.get_entry(ordn)), "ordinal": ordn})

    imports = {}
    for i in range(ida_nalt.get_import_module_qty()):
        mod = ida_nalt.get_import_module_name(i) or f"module_{i}"
        names = []

        def cb(ea, name, ordinal, names=names):
            names.append(name or f"#{ordinal}")
            return True

        ida_nalt.enum_import_names(i, cb)
        imports[mod] = names

    funcs = []
    for ea in idautils.Functions():
        f = ida_funcs.get_func(ea)
        funcs.append((f.size(), ea))
    funcs.sort(reverse=True)
    biggest = [{"name": ida_funcs.get_func_name(ea), "ea": hex(ea), "size": size} for size, ea in funcs[:60]]

    strings = []
    for s in idautils.Strings():
        if len(strings) >= int(job.get("max_strings", 400)):
            break
        text = str(s)
        if len(text) >= 5:
            strings.append({"ea": hex(s.ea), "s": text[:MAX_STR]})

    return {
        "info": info, "segments": segs, "entries": entries, "imports": imports,
        "function_count": len(funcs), "largest_functions": biggest, "strings": strings,
    }


def decompile(job):
    have_hr = ida_hexrays.init_hexrays_plugin()
    limit = int(job.get("max_chars_each", 8000))
    out = []
    for t in job.get("functions", []):
        ea = resolve_any(t)
        f = ida_funcs.get_func(ea) if ea is not None else None
        if f is None:
            out.append({"target": t, "error": "no function at target"})
            continue
        item = {"target": t, "ea": hex(f.start_ea), "name": ida_funcs.get_func_name(f.start_ea)}
        text = None
        if have_hr:
            try:
                text = str(ida_hexrays.decompile(f.start_ea))
                item["kind"] = "pseudocode"
            except Exception as e:  # decompilation failures are common on obfuscated code
                item["decompile_error"] = str(e)
        if text is None:
            lines = []
            for head in idautils.FuncItems(f.start_ea):
                lines.append(f"{head:x}  " + ida_lines.tag_remove(ida_lines.generate_disasm_line(head, 0) or ""))
            text = "\n".join(lines)
            item["kind"] = "disassembly"
        item["text"] = text[:limit]
        item["truncated"] = len(text) > limit
        out.append(item)
    return {"hexrays": bool(have_hr), "functions": out}


def xrefs(job):
    out = []
    for t in job.get("targets", []):
        ea = resolve_any(t)
        if ea is None:
            out.append({"target": t, "error": "unresolved"})
            continue
        refs = []
        for x in idautils.XrefsTo(ea, 0):
            fn = ida_funcs.get_func(x.frm)
            refs.append({
                "from": hex(x.frm),
                "function": ida_funcs.get_func_name(fn.start_ea) if fn else None,
                "type": idautils.XrefTypeName(x.type),
            })
            if len(refs) >= 300:
                break
        out.append({"target": t, "ea": hex(ea), "xrefs": refs})
    return {"targets": out}


def script(job):
    ns = {"idaapi": idaapi, "idc": idc, "idautils": idautils, "ida_bytes": ida_bytes,
          "ida_funcs": ida_funcs, "ida_hexrays": ida_hexrays, "ida_name": ida_name, "result": None}
    exec(compile(job["code"], "<agent_script>", "exec"), ns)
    return {"result": ns.get("result")}


ACTIONS = {"overview": overview, "decompile": decompile, "xrefs": xrefs, "script": script}


def main():
    job_path = idc.ARGV[1]
    with open(job_path, "r", encoding="utf-8") as f:
        job = json.load(f)
    try:
        ida_auto.auto_wait()
        payload = {"ok": True, "action": job["action"], "data": ACTIONS[job["action"]](job)}
    except Exception:
        payload = {"ok": False, "action": job.get("action"), "error": traceback.format_exc()[-6000:]}
    with open(job["output"], "w", encoding="utf-8") as f:
        json.dump(payload, f, default=str)


try:
    main()
finally:
    ida_kernwin.msg("harness done\n")
    idc.qexit(0)
