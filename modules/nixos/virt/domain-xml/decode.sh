#!/usr/bin/env nix-shell
#!nix-shell -i bash -p dmidecode python3

set -euo pipefail

# key: `dmidecode -s [keyword]`
declare -A FIELDS=(
    [BIOS_VENDOR]=bios-vendor
    [BIOS_VERSION]=bios-version
    [BIOS_DATE]=bios-release-date
    [BIOS_REVISION]=bios-revision
    [SYS_MANUFACTURER]=system-manufacturer
    [SYS_PROD_NAME]=system-product-name
    [SYS_VERSION]=system-version
    [SYS_SERIAL]=system-serial-number
    [SYS_UUID]=system-uuid
    [SYS_SKU]=system-sku-number
    [SYS_FAMILY]=system-family
    [BOARD_MANUFACTURER]=baseboard-manufacturer
    [BOARD_PRODUCT]=baseboard-product-name
    [BOARD_VERSION]=baseboard-version
    [BOARD_SERIAL]=baseboard-serial-number
    [CHASSIS_MANUFACTURER]=chassis-manufacturer
    [CHASSIS_VERSION]=chassis-version
    [CHASSIS_SERIAL]=chassis-serial-number
    [CHASSIS_ASSET_TAG]=chassis-asset-tag
)
for key in "${!FIELDS[@]}"; do
    val=$(dmidecode -s "${FIELDS[$key]}" 2>/dev/null | tr -d '\n' || true)
    case "$key" in
        SYS_UUID|SYS_SERIAL|BOARD_SERIAL)
            [[ -n $val ]] || { echo "ERR empty '$key' from dmidecode" >&2; exit 1; } ;;
    esac

    # store retrieved value in shell environment
    export "SUB_$key=$val"
done

oem_raw=$(dmidecode -t 11 2>/dev/null || true)
export SUB_OEM_RAW="$oem_raw"

python3 - "$@" <<'PY'
import os, re, sys, html
template = r'''<bios>
    <entry name="vendor">@@BIOS_VENDOR@@</entry>
    <entry name="version">@@BIOS_VERSION@@</entry>
    <entry name="date">@@BIOS_DATE@@</entry>
    <entry name="release">@@BIOS_REVISION@@</entry>
</bios>
<system>
    <entry name="manufacturer">@@SYS_MANUFACTURER@@</entry>
    <entry name="product">@@SYS_PROD_NAME@@</entry>
    <entry name="version">@@SYS_VERSION@@</entry>
    <entry name="serial">@@SYS_SERIAL@@</entry>
    <entry name="uuid">@@SYS_UUID@@</entry>
    <entry name="sku">@@SYS_SKU@@</entry>
    <entry name="family">@@SYS_FAMILY@@</entry>
</system>
<baseBoard>
    <entry name="manufacturer">@@BOARD_MANUFACTURER@@</entry>
    <entry name="product">@@BOARD_PRODUCT@@</entry>
    <entry name="version">@@BOARD_VERSION@@</entry>
    <entry name="serial">@@BOARD_SERIAL@@</entry>
    <entry name="asset">Default string</entry>
    <entry name="location">Default string</entry>
</baseBoard>
<chassis>
    <entry name="manufacturer">@@CHASSIS_MANUFACTURER@@</entry>
    <entry name="version">@@CHASSIS_VERSION@@</entry>
    <entry name="serial">@@CHASSIS_SERIAL@@</entry>
    <entry name="asset">@@CHASSIS_ASSET_TAG@@</entry>
    <entry name="sku">Default string</entry>
</chassis>
<oemStrings>@@OEM_STRINGS@@
</oemStrings>
'''

# sub scalar fields from SUB_* env vars (escape xml for each value)
def sub(m):
    key = m.group(1)
    if key == "OEM_STRINGS":
        return m.group(0)
    val = os.environ.get("SUB_" + key, "")
    return html.escape(val, quote=True)

out = re.sub(r'@@([A-Z_]+)@@', sub, template)
oem_vals = re.findall(r'^\s*(String \d+:\s?.*)$', os.environ.get("SUB_OEM_RAW", ""), re.MULTILINE)
oem_block = "".join(
    "\n    <entry>{}</entry>".format(html.escape(v, quote=True))
    for v in oem_vals if v.strip() != ""
)
out = out.replace("@@OEM_STRINGS@@", oem_block)
sys.stdout.write(out)
PY
