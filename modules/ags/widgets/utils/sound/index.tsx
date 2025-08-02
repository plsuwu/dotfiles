import { createBinding } from "ags";
import { Gtk } from "ags/gtk4";
import AstalWp from "gi://AstalWp?version=0.1";

export default function Sound() {
  const { defaultSpeaker: speaker } = AstalWp.get_default()!;

  return (
    <menubutton can_focus={false} cssClasses={["speaker-icon"]}>
      <image pixel_size={12} iconName={createBinding(speaker, "volumeIcon")} />
      <popover has_arrow={false} cssClasses={["popover"]}>
        <box>
          <slider
            widthRequest={200}
            heightRequest={2}
            onChangeValue={({ value }) => speaker.set_volume(value)}
            value={createBinding(speaker, "volume")}
          />
        </box>
      </popover>
    </menubutton>
  );
}
