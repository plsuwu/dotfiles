import app from "ags/gtk4/app";
import Sound from "../utils/sound";

import GLib from "gi://GLib";
import Astal from "gi://Astal?version=4.0";
import Gtk from "gi://Gtk?version=4.0";
import Gdk from "gi://Gdk?version=4.0";

import { createState } from "ags";
import { createPoll } from "ags/time";
import ActivityHandler, { gdkToHyprland } from "./workspaces";

function Clock({ timeHFmt = "%H", timeMFmt = "%M %P", dateFmt = "%a %d %b" }) {
  const [date, setDate] = createState("");
  const [hours, setHours] = createState("");
  const [mins, setMins] = createState("");

  const timer = createPoll("", 1000, () => {
    const local = GLib.DateTime.new_now_local();

    const hours = local.format(timeHFmt)!;
    const mins = local.format(timeMFmt)!;
    const date = local.format(dateFmt)!;

    setDate(date);
    setHours(hours);
    setMins(mins);
    return "";
  });

  return (
    <box valign={Gtk.Align.CENTER} class="Clock" cssClasses={["clock"]}>
      {/*
        `timer` is an empty string and this label exists only to
        hold the poller open 
      */}
      <label label={timer} />
      <menubutton has_frame={true} can_focus={false}>
        <label label={date} cssClasses={["date"]} />
        <popover cssClasses={["popover"]} has_arrow={false}>
          <Gtk.Calendar />
        </popover>
      </menubutton>
      <box cssClasses={["time"]}>
        <label label={hours} />
        <label label=":" cssClasses={["animate-blink"]} />
        <label label={mins} />
      </box>
    </box>
  );
}

export default function Bar(gdkmonitor: Gdk.Monitor) {
  const { TOP, LEFT, RIGHT } = Astal.WindowAnchor;
  const hMonitor = gdkToHyprland(gdkmonitor);

  return (
    <window
      visible
      name="bar"
      gdkmonitor={gdkmonitor}
      exclusivity={Astal.Exclusivity.EXCLUSIVE}
      anchor={TOP | LEFT | RIGHT}
      application={app}
    >
      <centerbox valign={Gtk.Align.CENTER}>
        <box $type="start">{ActivityHandler(hMonitor)}</box>
        <box $type="center"></box>
        <box $type="end">
          <Sound />
          <Clock />
          {/* <Wireless /> */}
        </box>
      </centerbox>
    </window>
  );
}
