{
  description = "templates";

  outputs =
    { self }:
    {
      templates = {
        c = {
          path = ./c;
        };
      };

      templates.default = self.templates.c;
    };
}
