import { createState, createBinding } from "ags";
import { Gdk, Gtk } from "ags/gtk4";
import Hyprland from "gi://AstalHyprland";

const hyprland = Hyprland.get_default();

function isWorkspaceOnMonitor(mIndex: number, ws: Hyprland.Workspace): boolean {
  return ws.id >= mIndex * 10 + 1 && ws.id < mIndex * 10 + 10;
}

export function hmFromGm(gm: Gdk.Monitor): Hyprland.Monitor {
  return hyprland
    .get_monitors()
    .find((mon) => mon.name === gm.get_connector())!;
}

export function activeClient(mIndex: number) {
    const lastWorkspaceId = createState()
}
