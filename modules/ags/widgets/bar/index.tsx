import app from "ags/gtk4/app";
import Sound from "../utils/sound";

import GLib from "gi://GLib";
import Astal from "gi://Astal?version=4.0";
import Gtk from "gi://Gtk?version=4.0";
import Gdk from "gi://Gdk?version=4.0";
import AstalNetwork from "gi://AstalNetwork";
import AstalTray from "gi://AstalTray";
import AstalMpris from "gi://AstalMpris";
import AstalApps from "gi://AstalApps";
import { For, With, createBinding, createState } from "ags";
import { createPoll } from "ags/time";
import { execAsync } from "ags/process";
import { hmFromGm, gmFromHm } from "./workspace";

function Tray() {
  const tray = AstalTray.get_default();
  const items = createBinding(tray, "items");

  items.get().forEach((item) => {
    print("io", item);
  });

  const init = (btn: Gtk.MenuButton, item: AstalTray.TrayItem) => {
    btn.menuModel = item.menuModel;
    btn.insert_action_group("dbusmenu", item.actionGroup);
    item.connect("notify::action-group", () => {
      btn.insert_action_group("dbusmenu", item.actionGroup);
    });
  };

  return (
    <box valign={Gtk.Align.CENTER}>
      <For each={items}>
        {(item) => (
          <menubutton $={(self) => init(self, item)}>
            <image gicon={createBinding(item, "gicon")} />
            {/* <label label={createBinding(item, "title")} /> */}
          </menubutton>
        )}
      </For>
    </box>
  );
}

// function Wireless() {
//   const network = AstalNetwork.get_default();
//   const wifi = createBinding(network, "wifi");
//
//   const sorted = (arr: Array<AstalNetwork.AccessPoint>) => {
//     return arr
//       .filter((ap) => !!ap.ssid)
//       .sort((a, b) => b.strength - a.strength);
//   };
//
//   async function connect(ap: AstalNetwork.AccessPoint) {
//     // connecting to ap is not yet supported
//     // https://github.com/Aylur/astal/pull/13
//     try {
//       await execAsync(`nmcli d wifi connect ${ap.bssid}`);
//     } catch (error) {
//       // you can implement a popup asking for password here
//       console.error(error);
//     }
//   }
//
//   return (
//     <box visible={wifi(Boolean)}>
//       <With value={wifi}>
//         {(wifi) =>
//           wifi && (
//             <menubutton>
//               <image iconName={createBinding(wifi, "iconName")} />
//               <popover>
//                 <box orientation={Gtk.Orientation.VERTICAL}>
//                   <For each={createBinding(wifi, "accessPoints")(sorted)}>
//                     {(ap: AstalNetwork.AccessPoint) => (
//                       <button onClicked={() => connect(ap)}>
//                         <box spacing={4}>
//                           <image iconName={createBinding(ap, "iconName")} />
//                           <label label={createBinding(ap, "ssid")} />
//                           <image
//                             iconName="object-select-symbolic"
//                             visible={createBinding(
//                               wifi,
//                               "activeAccessPoint",
//                             )((active) => active === ap)}
//                           />
//                         </box>
//                       </button>
//                     )}
//                   </For>
//                 </box>
//               </popover>
//             </menubutton>
//           )
//         }
//       </With>
//     </box>
//   );
// }

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

  const hm = hmFromGm(gdkmonitor);
  const gm = gmFromHm(hm);

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
        <box $type="start">
          <Tray />
          {/* <Mpris /> */}
        </box>
        <box $type="center">
          <label label="asd" />
        </box>
        <box $type="end">
          <Sound />
          <Clock />
          {/* <Wireless /> */}
        </box>
      </centerbox>
    </window>
  );
}
