import { Gdk, Gtk } from "ags/gtk4";
import Hyprland from "gi://AstalHyprland";

export const AVAILABLE_WORKSPACES = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
export const hyprland = Hyprland.get_default();

export function gdkToHyprland(gdkMonitor: Gdk.Monitor) {
  return hyprland
    .get_monitors()
    .find((monitor) => monitor.name === gdkMonitor.connector)!;
}

export function isWsOnMonitor(
  monitor: Hyprland.Monitor,
  workspace: Hyprland.Workspace,
) {
  if (workspace && workspace.monitor) {
    return monitor.id === workspace.monitor.id;
  } else {
    return false;
  }
}

import ClientActivity from "./client-activity";
import WorkspaceActivity from "./workspace-activity";

export default function ActivityHandler(monitor: Hyprland.Monitor) {
  return (
    <box cssClasses={["Activity"]}>
      {WorkspaceActivity(monitor)}
      {ClientActivity(monitor)}
    </box>
  );
}

// export { ClientActivity, WorkspaceActivity };
