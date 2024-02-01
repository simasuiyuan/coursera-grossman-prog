## The most important commands in Emacs:
1. `C-x C-c`: Quit Emacs
2. `C-g`: Cancel the current action
3. `C-x C-f`: Open a file (whether or not it already exists)
4. `C-x C-s`: Save a file
5. `C-x C-w`: Write a file (probably more familiar to you as Save as...)

7. `C-x k` ( kill-buffer )
8.  `C-x 0`: Close window
10. `C-x u or C-_`: undo

## REPL (SML)
6. `C-c C-s`: Open SML buffer
9. `C-d`: close process sml (do this when refresh the *.sml file)

### Cut, copy, paste:
* Highlight text with the mouse or by hitting `C-Space` to set a mark and then moving the cursor to highlight a region.
* `C-w`: Cut a highlighted region
* `M-w`: Copy a highlighted region
* `C-k`: Cut (kill) from the cursor to the end of the line
* `C-y`: Paste (yank)

### Some other useful commands:
* `C-x 2`: Split the window into 2 buffers, one above the other (Use the mouse or `C-x o` to switch between them)
* `C-x 0`: Undo window-splitting so there is only 1 buffer
* `C-x b`: Switch to another buffer by entering its name
* `C-x C-b`: See a list of all current buffers

### Getting help within Emacs: In addition to the help button/menu on the right...
* `C-h`: Hitting this will display a short message in the minibuffer:  `C-h` (Type ? for further options)
*  `C-h b`: Key bindings. This lists all key bindings that are valid for the current mode. Note that key bindings change from mode to mode.
* `C-h a`: Command apropos. After typing `C-h` a you can type a symbol and a buffer will appear that lists all symbols and functions that match that phrase.





