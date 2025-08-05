{
  description = "templates";

  outputs =
    { self }:
    {
      templates = {
        c = {
          path = ./c;
        };

        cpp = {
          path = ./cpp;
        };
      };

      templates.default = self.templates.c;
    };
}
