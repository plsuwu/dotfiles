import { Accessor, createBinding, createState, For, State } from "ags";
import { AVAILABLE_WORKSPACES, hyprland, isWsOnMonitor } from ".";
import { Gtk } from "ags/gtk4";
import Hyprland from "gi://AstalHyprland";

export default function ClientActivity(monitor: Hyprland.Monitor) {
  const [prev, setPrev] = createState("");
  const client = createBinding(hyprland, "focused_client").as((client) => {
    if (isWsOnMonitor(monitor, client.workspace)) {
      setPrev(client.initial_title);
    }

    return prev.get();
  });

  return (
    <box cssClasses={["clients"]}>
      <label label={client} />
    </box>
  );
}
