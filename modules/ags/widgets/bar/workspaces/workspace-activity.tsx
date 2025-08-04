import { createBinding, createState, For } from "ags";
import { AVAILABLE_WORKSPACES, hyprland, isWsOnMonitor } from ".";
import Hyprland from "gi://AstalHyprland";
import { Gtk } from "ags/gtk4";

export default function WorkspaceActivity(monitor: Hyprland.Monitor) {
  const mapped = createBinding(hyprland, "workspaces").as((workspaces) => {
    workspaces = workspaces.filter((ws) => isWsOnMonitor(monitor, ws));
    return AVAILABLE_WORKSPACES.map((wsn, idx) => {
      return workspaces.find((ws) => ws.id === wsn) ?? idx;
    });
  });

  return (
    <box valign={Gtk.Align.CENTER} cssClasses={["workspaces"]}>
      <For each={mapped}>
        {(ws, idx) => {
          if (typeof ws == "number") {
            return (
              <label label={String(idx.get() + 1)} cssClasses={["inactive"]} />
            );
          } else {
            return <label label={String(ws.id)} cssClasses={["focused"]} />;
          }
        }}
      </For>
    </box>
  );
}
