# Soluna Documentation

Hey stranger, you just opened a book that contains receipes to make mystic programs.

Lisp is an historical language that helped many developpers to solve very complex problems. Lisp is one of the foundation for modern programming.

## So why create yet another Lisp dialect ?

First, it's fun. Doing it wasn't a pain, it was a very pleasant journey. I finally learned how Lisp works and why it is so great. I also learned why people tend to not like it that much.

Soluna solves (or atleast try) some "bad" ideas that are present in classic Lisp. It also doesn't try to go too far from the origins. What would be the point anyways.

Second, Lisp dialects feels kinda wrong to me. They are great, but I wanted MY OWN dialect. I wanted something light, cross-platform, and that can be learned easily.

It's so barebone, that you will know EXACTLY what the code you wrote do to solve your problem.

## Use cases

Mostly, problem solving. You have an input, you need an output. That's it. Soluna will not let you code a fancy X11 app, an RPG, an IRC server. No ! It will let you solve mathematical or coding problems, and things in that range.

You want to find every occurence of a number in a list and get their positions ? Soluna is made for you.

You want to learn a bit of functionnal programming ? Soluna is made for you.

You want to make money with coding ? Learn React.

## Where to start ?

You can use the builtin REPL called Oracle. Just start Soluna without argument to start it magically.

You'll be able to run S-Expressions and see the result of it.

The source code for Oracle is present (and up to date) in the examples folder.

## How to read Soluna syntax

Pretty easy, (function arg1 arg2 ...). This is an S-Expression. To understand it a bit more, let's see how to add two numbers:

`(+ 35 34)` -> The expression is the full parenthesis, the function is '+' which is add, and the arguments are 35 and 34.

This looks strange ? This is just reverse polish notation. It can look strange at first but it become easy to read after a bit of time. Little tip, it's like reading the sentence "Adding 35 to 34".

`(+ 32 (+ 34 3))` -> You read this like: "Add 32 to the addition of 34 and 3".

## What are the builtin tools ?

Builtin tools helps you do more stuff in an easier maner. You can look at the [Language Reference](https://github.com/l0wigh/Soluna/blob/master/LANGUAGE.md)

Missing something ? You should be able to code what is missing by yourself. Casting spells sometimes requires you to be smart.

In fact, what is present here is enough to create any kind of functions. You might need to find tricks to do what you want, but it helps you understand everything you do.

Also you can comment your code using this format `; This is a line comment`.

## Parenthesis are hard to read

I know that it can be daunting at first, but it's honestly not that hard. Soluna isn't indentation based, so you can make your code look as you want. You just need to respect the parenthesis rules and you are fine !

Look at this monstruosity:

```lisp
(

defvar x (+
3 4)
        )
(       writeln             x)
```

This code works, without any issues. while it's unreadable. Your code will look like you want, parenthesis are just a way to let Soluna understand where is the start and where is the end of the expression.

Also it works nice in (Neo)vim for example, just use the normal % command to navigate around an expression. It feels great right ?

## Where should I start ?

Honestly, just do some simple list manipulation to understand how the language works, you should honestly be able to understand how everything works in almost no time.

Want a little challenge ? Do a list with numbers in it and print the numbers multiply by 2. Bonus point if you generate the list without manually writing the numbers by yourself

You can also look at examples. They are mostly commented and will help you get how Soluna works. Let me just give you a little "Hello World !"

```lisp
; Defining a genlist function that create a list from start to end (included) using recursion
(function genlist (start end) (
	if (> start end)
		(list)
		(cons start (genlist (+ start 1) end))
))

; Main function that will run multiple expression
(function main () (do
	(defvar hello_msg "Hello World from Soluna !") ; Defining the hello_msg variable

	; Defining power lambda function that will be present only in the main scope
	(defvar power (lambda (val pow acc) (
		if (= pow 1)
			acc
			(power val (- pow 1) (* acc val))
		)
	))
	; Use the power function to create the variable x
	(defvar x (power 2 3 2))

	; Create a variable that takes it's values from genlist and then pass it through map
	; As you can see, even complicated code like this is still easy to work with
	(defvar y (map (lambda (x) (* x 2)) (genlist 1 10)))

	; Print hello_msg and x
	(writeln hello_msg)
	(write "2 pow 3 = ")
	(writeln x)
	(write "map and lambda function on a generated list from 1 to 10 = ")
	(writeln y)

	69 ; Return value. Useless. It just to demonstrate how do keyword works
))

; Calling the main function and print the return value
(defvar x (main))
(write "Main return value: ")
(writeln x)

; The error here is expected, it shows how the defvar scope works
(power 2 3 2)
```

## Last words

Lisp and all the dialects that comes from it might not be perfect or as powerful as other languages. That's ok. Just have fun !
