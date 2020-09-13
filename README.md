# Picasso
Picasso is an Abstract element drawing library which provides several
ways of drawing abstract element. As abstract elements may be
unbounded, and are generally defined on more than 2 dimensions,
rendering them in a 2d context can be difficult.  Picasso handles most
of the boilerplate you usually write to draw abstract elements and
allows you to view thos with minimum effort from your end.

### Documentation
you can build it locally by doing ``make doc`` or consult the online [documentation](https://ghilesz.github.io/picasso/picasso/index.html)

### Different backends
Picasso features different ways of visualizing abstract elements:
- interactive [Gtk](http://lablgtk.forge.ocamlcore.org/) window (scrollable, zoomable) 
- LaTex generation
- 3D .obj file generation
<!-- - Non-interractive [graphics](https://github.com/ocaml/graphics) window (todo) -->
