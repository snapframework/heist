Attribute Splices
=================

Attribute splices are new in Heist 0.10.  They solve the problem of wanting to
be able to dynamically make empty attributes appear or disappear with a splice
without binding a splice to the whole tag.  This issue comes up most
frequently when dealing with empty attributes such as HTML's "disabled" or
"checked".

> module Heist.Tutorial.AttributeSplices where
> import           Heist.Tutorial.Imports

Consider a page with several radio buttons.  You want the correct one to be
selected based on the value of a parameter in the HTTP request.  The HTML
would look something like this:

    <input type="radio" name="color" value="red" checked>Red</input>
    <input type="radio" name="color" value="green">Green</input>
    <input type="radio" name="color" value="blue">Blue</input>

We want to automatically generate the "checked" attribute appropriately.  This
could be done with a splice bound to the input tag, but there might be a
number of other input tags on the page, so your splice would at best be
executed on more tags than necessary and at worst not have the granularity
necessary to work properly.  The ${} syntax for splices inside of attribute
values also won't work because it can only affect an attribute's value.  It
can't make the attribute disappear entirely.  This problem can be solved
nicely with attribute splices that have the following type:

< type AttrSplice m = Text -> m [(Text, Text)]

An attribute splice is a computation in the runtime monad that takes the value
of the attribute it is bound to as its argument and returns a list of
attributes to substitute back into the tag.  Here's how we might implement a
splice to solve the above problem.

> autocheckedSplice :: Text -> StateT Text IO [(Text, Text)]
> autocheckedSplice v = do
>     val <- get -- app-specific retrieval of the appropriate value here
>     let checked = if v == val
>                     then [("checked","")]
>                     else []
>     return $ ("value", v) : checked

In this toy example we are using `StateT Text IO` as our "runtime" monad where
the Text state holds the value of the radio button that should be checked.  We
assume that the current value we're checking against is passed as the bound
attribute's value, so we compare that against the value to be checked.  Then
we return a list with the appropriate value and the checked attribute if
necessary.  We bind this splice to the "autocheck" attribute by adding it to
the hcAttributeSplices list in HeistConfig.

To make everything work we use the following markup for our radio buttons:

    <input type="radio" name="color" autocheck="red">Red</input>
    <input type="radio" name="color" autocheck="green">Green</input>
    <input type="radio" name="color" autocheck="blue">Blue</input>

