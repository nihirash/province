# Province Web engine

Province web engine - is basic set of helpers to make your web apps in Chicken scheme easly.

Province web engine under construction now, but you can feel free to modify/send me PR.

## Requirements

Of course, You need Chicken Scheme(I use Chicken 4.11.0) and several libraries:

 * sql-de-lite
 * awful
 * awful-sql-de-lite
 * utf-8

I've using sqlite only now(I really think it will be enought for me). 

## Running

If you installed all requirements run migrate.scm - it creates basic db file.

And then - just run ```awful main.scm``` or ```awful main.scm --development-mode```

I think use it with some frontend like nginx/caddy.

## Post scriptum

Please make something good and have a happy hacking!
