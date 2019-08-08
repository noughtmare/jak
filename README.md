![](jak-logo.svg)

Simple frp text editor for testing and learning purposes.

## Status

The text editor can currently edit and show text.

Planned features are (in order of importance):

  - [x] Can edit text.
  - [x] Has a viewport.
  - [ ] Supports line wrapping.
  - [ ] Can read and write files.

Long term goals:
 
  - [ ] Supports the Language Server Protocol (using [haskell-lsp-client](https://github.com/noughtmare/haskell-lsp-client)).
  - [ ] Has vim keybindings.
  - [ ] Has a browser frontend (this is the reason for the AGPL license).

## Design decisions

Feel free to contact me (for example by opening an issue here) if you would
like to have any other design decisions explained.

### Yampa

I chose to use frpnow mainly because it promises an efficient implementation
and it doesn't have many extraneous features which I probably won't need and
will only get in the way of learning how to use it.

The main alternatives for me would be Yampa, but I didn't want to learn how
to use the arrow interface. Or maybe reflex when I'm more comfortable with
FRP.

#### UPDATE 2019-08-08:

I have now learned how to use the arrow interface, it is not too hard. I think
that the new Yampa implementation is much nicer than the previous frpnow
implementation.

### Type safety

I am using explicit data types for each different type of integer. I hope
this will prevent most mistakes that can happen.

The main disadvantage of this is that conversions between the types need
to be explicitely specified by the programmer, but I personally really
like it when all the types logically fit together.

### Modularity

I have decided to split the viewport, cursor and content. This should
improve the readability and modularity of the code. A disadvantage is
that some internal information is lost so some functions may have
worse performance, but it shouldn't influence the worst case time
complexity.

When implementing this I really noticed that the code became much more
readable.
