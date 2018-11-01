# B-machine 

This is small research project with a traget to understand a structure of a software PLC

## TODO

* Add a block "AssignIf"
* Add a block "Select2"
* Add a block "Mux"
* Add a block "Demux"
* Add a block "RS-trigger"
* Add a block "Rising Edge"
* Add a block "Falling Edge"
* Add the blocks "Compare"

## Next steps

Now I can implement a relative simple program, even I can say a linear program.
But some things are missing:

+ Branches (Jump)
+ Subroutines
+ Hot changing the data memory
+ Hot changing the program

I don't know yet how to implement this within my model :)

## Future

+ Add the variable attributes: minimum, maxumum, physical unit
+ Check a consistency
+ Make a simple translator from a text

## Notes

I worry about an overflow due the integers operations. I disabled an overflow internal check because the Ada exceptions are very slow.
There is an [article](http://www.cplusplus.com/articles/DE18T05o/) which describes how to handle the overflow.
May be I can implement the special blocks to check is it the operations are safe. 
The output will be used to do a jump over.
