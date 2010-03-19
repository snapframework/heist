# Snap Templates

Snap templates serve two primary design goals.  First, they facilitate the
separation of the view from the other aspects of your application.  Second,
they provide abstraction capabilities that allow you to avoid repeated template
code.  This allows you to follow the DRY principle (Don't Repeat Yourself) in
the development of your application views.

Snap has two primary template abstraction constructs: bind and apply.
They are implemented as specialized XML tags in the snap namespace.

## `<snap:bind ...>`

The `snap:bind` tag allows you to bind XML content to a single tag.
Whenever the bound tag is used, the template engine will substitute
the content in its place.  This allows you to essentially create your
own higher-level markup language that snap transforms into whatever
XML-based markup language is native to your application.

### Attributes

The `snap:bind` tag has a single required attribute called `tag` specifying the
name of the bound tag.  If this attribute is not present, then the
`snap:bind` tag has no effect.

### Example

Here's a simple example demonstrating the use of bind.

~~~~~~~~~~~~~~~ {.html}
  <snap:bind tag="longname">
    Einstein, Feynman, Heisenberg, and Newton Reasearch Corporation
Ltd.<sup>TM</sup>
  </snap:bind>
  We at <longname/> have research expertise in many areas of physics.
Employment at <longname/> carries significant prestige.  The rigorous
hiring process developed by <longname/> is leading the industry.
~~~~~~~~~~~~~~~

The full company name will be substituted at every occurrance of the
`<longname/>` tag.  This eliminates repetition and makes it easier to
make changes.

## `<snap:apply ...>`

The `snap:apply` tag loads one of your application templates and inserts it
into the current template's XML tree.  If the target template does not have any
special tags, then the contents of the `snap:apply` tag are ignored.

### Attributes

The `snap:apply` tag has one required attribute called `template`.  This attribute
specifies the path to the template being applied. **`TODO`**: describe template
path behavior.


### Example

Let's look at a simple example to demonstrate the most basic use of the
`snap:apply` tag.  Say you have a navigation menu that is used on many
different pages of your site.  You want to avoid duplicating the HTML code in
multiple different page templates, so you might put it in a template file by
itself called `nav.tpl` that looks like this:

~~~~~~~~~~~~~~~ {.html}
  <ul>
    <li><a href="/">Home</a></li>
    <li><a href="/faq">FAQ</a></li>
    <li><a href="/contact">Contact</a></li>
  </ul>
~~~~~~~~~~~~~~~

Then to include this nav template in your front page template, you would use
the `snap:apply` tag.  Here is what a simple home page template `home.tpl`
might look like:

~~~~~~~~~~~~~~~ {.html}
  <html>
    <head>
      <title>Home Page</title>
    </head>
    <body>
      <h1>Home Page</h1>
      <snap:apply template="nav"/>
      <p>Welcome to our home page</p>
    </body>
  </html>
~~~~~~~~~~~~~~~

When a user requests the `/home` URL, Snap would serve `home.tpl` (**`FIXME`**:
depends on the snaplet in question), and the nav template would automatically
be inserted into the page.  Here is what the HTML will look like after the Snap
processes the template:

~~~~~~~~~~~~~~~ {.html}
  <html>
    <head>
      <title>Home Page</title>
    </head>
    <body>
      <h1>Home Page</h1>
      <ul>
        <li><a href="/">Home</a></li>
        <li><a href="/faq">FAQ</a></li>
        <li><a href="/contact">Contact</a></li>
      </ul>
      <p>Welcome to our home page</p>
    </body>
  </html>
~~~~~~~~~~~~~~~


## `<snap:get ...>`

Sometimes it is useful to pass information (usually in the form of XML data)
into the template when it is applied so the template can insert it in useful
places.  This allows you to build page templates that are not just static
blocks of code.  If you are a programmer, you can think of a template as if it
was a function that could have any number of parameters.

In our previous example, we did not pass any parameters to the `nav` template
when it was applied, so the `<snap:apply>` tag was empty.  If we include data
inside the body of the `<snap:apply>` tag, the template being called can access
this data with the `<snap:get>` tag.  The following simple example illustrates
this concept.  We create a site template called `default.tpl`:

~~~~~~~~~~~~~~~ {.html}
<html>
  <head>
    <title>Home Page</title>
  </head>
  <body>
    <div id="header">
      <h1>XYZ Inc.</h1>
    </div>
    <div id="content">
      <snap:get />
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~


The `<snap:get>` tag "pulls in" the page content from the calling template and
inserts it into the content `<div>`.

Now we have a template for our home page called home.tpl:

~~~~~~~~~~~~~~~ {.html}
<snap:apply template="default">
  <h1>Home Page</h1>
  <p>Welcome to XYZ Inc</p>
</snap:apply>
~~~~~~~~~~~~~~~

And when Snap receives a request to `/home`, it will serve the following:

~~~~~~~~~~~~~~~ {.html}
<html>
  <head>
    <title>Home Page</title>
  </head>
  <body>
    <div id="header">
      <h1>XYZ Inc.</h1>
    </div>
    <div id="content">
      <h1>Home Page</h1>
      <p>Welcome to XYZ Inc</p>
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~

The two lines from inside the `<snap:apply>` tag have been substituted into the
content div in `default.tpl`.  Notice the difference between these two
examples.  In the first example we pulled in a template (`nav.tpl`) that went
inside the page being served (`home.tpl`).  In the second example, `home.tpl`
is still the intended target of requests, but the `default.tpl` template
surrounds the content that home.tpl supplies as an argument.  This seems like
different behavior, but it is just a different use of the same `apply` tag.  This
illustrates the power of a simple concept like `apply`.

## Using Bind and Apply

What if, in the above example, we decided that the contents of the header div
should be different for different pages?  To do this, we need a way to pass
multiple parameters into a template.  Snap provides this capability with the
`<snap:bind>` tag.  Inside the body of a `<snap:apply>` tag, you can have
multiple snap:bind tags surrounding data to be passed as separate parameters.
Each `<snap:bind>` tag must have a name attribute that provides a name for its
contents.  Then, inside the template, each `<snap:get>` tag should have a name
attribute indicating the name of the parameter to which it corresponds.

The previous example only needs a few modifications to default.tpl to allow
multiple parameters.  The `<snap:get>` tags now have name attributes:

~~~~~~~~~~~~~~~ {.html}
<html>
  <head>
    <title>Home Page</title>
  </head>
  <body>
    <div id="header">
      <header/>
    </div>
    <div id="content">
      <content/>
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~


And `home.tpl` uses the `<snap:bind>` tag with a name attribute to define values
for the parameters:

~~~~~~~~~~~~~~~ {.html}
<snap:apply name="default">
  <snap:bind tag="header">
    <h1>XYZ Inc.</h1>
  </snap:bind>
  <snap:bind tag="content">
    <h1>Home Page</h1>
    <p>Welcome to XYZ Inc</p>
  </snap:bind>
</snap:apply>
~~~~~~~~~~~~~~~

The result template for this example is the same as the previous
example.

## `<snap:ignore ...>`

In some cases you may want to include example data in a Snap template that
should not be rendered when the site is active.  Snap provides the
`<snap:ignore>` tag for this purpose.  All `<snap:ignore>` tags and their contents
will be eliminated in a template's output.

