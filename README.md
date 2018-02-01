# jak

Simple frp text editor for testing/learning purposes.

## Status

The text editor can currently edit and show text.

Planned features are (in order of importance):

  - [x] Having a viewport
  - [ ] Wrapping lines
  - [ ] Reading and writing files

Long term goals:
 
  - [ ] Integrate [haskell-lsp-client](https://github.com/noughtmare/haskell-lsp-client)
  - [ ] Vim keybindings
  - [ ] Browser frontend (this is the reason for the AGPL license)

## Design decisions

Feel free to contact me (for example by opening an issue here) if you would
like to have any other design decisions explained.

### Which FRP library should I use?

I chose to use frpnow mainly because it promises an efficient implementation
and it doesn't have many extraneous features which I probably won't need and
will only get in the way of learning how to use it.

The main alternatives for me would be Yampa, but I didn't want to learn how
to use the arrow interface. Or maybe reflex when I'm more confortable with
FRP.

### How to make sure I don't confuse the row, column, width or height?

I am using explicit data types for each different type of integer. I hope
this will prevent most mistakes that can happen.

The main disadvantage of this is that conversions between the types need
to be explicitely specified by the programmer, but I personally really
like it when all the types logically fit together.

### How to make the code more modular?

I have decided to split the viewport, cursor and content. This should
improve the readability and modularity of the code. A disadvantage is
that some internal information is lost so some functions may have
worse performance, but it shouldn't influence the worst case time
complexity.

When implementing this I really noticed that the code became much more
readable.
