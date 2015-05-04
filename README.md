# RacketTypeProvider
F#-style Type Providers in the Racket language

## What is a Type Provider?
A Type Provider is a construct to aid programmers that are
working with large and/or changing datasets, especially those
with no explicit type/shape definition such as an XML DTD.

Type Providers integrate with IDEs such as Visual Studio or
Dr. Racket to give autocompletion hints when a developers is
trying to access part of this data. Additionally, the functions
to get, create, or mutate pieces of this data are checked at
compile-time for validity. Finally, the compile-time bindings
to functions need to be available at runtime, even if the data
used at runtime has different contents than the data used while
editing.

In short the features of Type Providers are:

- Compile-time checking of data manipulation.
- IDE Autocompletion of names of data elements/fields.
- The ability to get the shape of data while editing code, and automatically adjust to the similarly-shaped contents of a different data source.

## Setup
The default Racket package management should be used to
install the info.rkt file.

After that, due to lame limitations that I totally should
fix, you will need to save any XML files you wish to use
to your /Racket folder OR use an absolute path name when
specifying the edit-time path within the macro call.

## How does it work?
This is a tricky question to answer. An exact answer can be
gleaned from the publicly available source code, but the
following may help with basic understanding. As with much
of this documentation, the example Type Provider used is
the XML Type Provider.

The user supplies an **edit-time** data source to a macro. This
is used to infer the type of the data by parsing the XML structure,
gathering the names of elements, the names of their fields,
and building structs out of them. This **edit-time** information
fuels the autocomplete, and supplies the expansion of the macro as
well.

The **edit-time** data is thus used by the macro in a normal
macro-expansion sense, but is also read by the IDE which performs
some of the same work as the macro, but does it without actually
expanding any macros. The IDE can now tell the user what names
of XML data (and their fields) are available with the prefix
the user started writing.

The macro, once expanded, will make available a **compile-time**
table of structs behind the scenes. This helps Dr. Racket's
background compilation recognize the XML data as structs as
sure as if you manually `define`d each of them yourself. An
additional contribution made at **compile-time** is the
generation of a `populate-at-runtime` function.

A user might not want the program to operate on the same data
that they used for **edit-time**. Perhaps the data that would
be useful to work with is updated every 15 minutes on some
remote server. In this case, the user can supply the path to
a potentially different set of data for use by the program at
**run-time**. So long as the shape does not vary too severely
from data supplied at **edit-time**, the Type Provider will
be able to accommodate this run-time data without any need to
revise the program every 15 minutes.  

## Wait, but standard Racket is not typed!
Racket may seem like a strange choice to implement a Type
Provider for. In fact, in this language the term is a bit
of a misnomer. While F# is able to give you compile-time
checking of types because it is an inherently typed language,

On the other hand, Racket can only give you the shape of the
data. Knowing the shape of your data means knowing which names
are accessible from within which object. Coupled with
autocompletion, this is still useful. (One pending feature of
this project is to give hints to the probable types of fields
based on the edit-time data.)

## Use
An example on how to use the Type Provider is available
in the xml-type-provider-use.rkt file. This will still
require you to copy the .txt file it references to /Racket
or modify it to use an absolute path.

## Contribution
Please feel free to contribute to this project via forking
this repo as with most Open Source Software. I will setup
better guidelines as this project continues to develop.
