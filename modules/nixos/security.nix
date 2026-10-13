{
  lib,
  ...
}:
{
  security.pam.services.hyprlock = { };

  security.polkit = {
    enable = true;
  };
}
