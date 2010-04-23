## Heist Templates

Heist templates serve two primary design goals.  First, they facilitate
the separation of the view from the other aspects of your application.
Second, they provide abstraction capabilities that allow you to avoid
repeated template code.  This allows you to follow the DRY principle
(Don't Repeat Yourself) in the development of your application views.

Heist has two primary template abstraction constructs: bind and apply.
They are implemented as specialized XML tags.

### The `<bind ...>` tag

The `bind` tag allows you to bind XML content to a single tag.
Whenever the bound tag is used, the template engine will substitute
the 'bind' tag's child nodes in its place.  This allows you to
essentially create your own higher-level markup language that Heist
transforms into whatever XML-based markup language is native to your
application.

#### Attributes

The `bind` tag has a single required attribute called `tag` specifying the
name of the bound tag.  If this attribute is not present, then the
`bind` tag has no effect.

#### Example

Here's a simple example demonstrating the use of bind.

~~~~~~~~~~~~~~~ {.html}
  <bind tag="longname">
    Einstein, Feynman, Heisenberg, and Newton Reasearch Corporation
Ltd.<sup>TM</sup>
  </bind>
  We at <longname/> have research expertise in many areas of physics.
Employment at <longname/> carries significant prestige.  The rigorous
hiring process developed by <longname/> is leading the industry.
~~~~~~~~~~~~~~~

The full company name will be substituted at every occurrance of the
`<longname/>` tag.  This eliminates repetition and makes it easier to
make changes.

### The `<apply ...>` tag

The `apply` tag loads one of your application templates and inserts it
into the current template's XML tree.  If the target template does not
have any special tags, then the contents of the `apply` tag are
ignored.

#### Attributes

The `apply` tag has one required attribute called `template`.  This
attribute specifies the name of the template being applied.  Heist
template names work a little differently from traditional paths and
filenames.

If the template name contains a '/' character, then it will behave
like traditional relative and absolute paths.  The root directory will
be the root of your template directory tree, and the current directory
will be the directory containing whatever template is currently being
processed.  Absolute template path names start at the root directory.
Relative template path names start at the current directory.

If the template name does not have any '/' characters, then Heist
searches in the current directory for a template with that name.  If
it finds one, then Heist applies the template just like you would
expect.  The different behavior is that if the named template is
not found in the current directory, Heist recursively searches up the
directory hierarchy looking for the name.  Heist uses the first
template it finds on the way up that has that name.  If no template is
found, then you'll get an error.

This cascading behavior allows you to put site-wide templates in the
top-level directory and selectively override them in subdirectories
for certain parts of your site.

#### Example

Let's look at a simple example to demonstrate the most basic use of
the `apply` tag.  Say you have a navigation menu that is used on many
different pages of your site.  You want to avoid duplicating the HTML
code in multiple different page templates, so you might put it in a
template file by itself called `nav.tpl` that looks like this:

~~~~~~~~~~~~~~~ {.html}
  <ul>
    <li><a href="/">Home</a></li>
    <li><a href="/faq">FAQ</a></li>
    <li><a href="/contact">Contact</a></li>
  </ul>
~~~~~~~~~~~~~~~

Then to include this nav template in your front page template, you
would use the `apply` tag.  Here is what a simple home page template
`home.tpl` might look like:

~~~~~~~~~~~~~~~ {.html}
  <html>
    <head>
      <title>Home Page</title>
    </head>
    <body>
      <h1>Home Page</h1>
      <apply template="nav"/>
      <p>Welcome to our home page</p>
    </body>
  </html>
~~~~~~~~~~~~~~~

When a user requests the `/home` URL, Heist would serve `home.tpl`,
and the nav template would automatically be inserted into the page.
Here is what the HTML will look like after Heist processes the
template:

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


### The `<content>` tag

Sometimes it is useful to pass information (usually in the form of XML
data) into the template when it is applied so the template can insert
it in useful places.  This allows you to build page templates that are
not just static blocks of code.  If you are a programmer, you can
think of a template as if it was a function that could have any number
of parameters.

In our previous example, we did not pass any parameters to the `nav`
template when it was applied, so the `<apply>` tag was empty.  If we
include data inside the body of the `<apply>` tag, the template being
called can access this data with the `<content>` tag.  The following
simple example illustrates this concept.  We create a site template
called `default.tpl`:

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
      <content />
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~


The `<content>` tag "pulls in" the page content from the calling
template and inserts it into the content `<div>`.

Now we have a template for our home page called home.tpl:

~~~~~~~~~~~~~~~ {.html}
<apply template="default">
  <h1>Home Page</h1>
  <p>Welcome to XYZ Inc</p>
</apply>
~~~~~~~~~~~~~~~

And when Heist receives a request to `/home`, it will serve the
following:

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

The two lines from inside the `<apply>` tag have been substituted into
the content div in `default.tpl`.  Notice the difference between these
two examples.  In the first example we pulled in a template
(`nav.tpl`) that went inside the page being served (`home.tpl`).  In
the second example, `home.tpl` is still the intended target of
requests, but the `default.tpl` template surrounds the content that
home.tpl supplies as an argument.  This seems like different behavior,
but it is just a different use of the same `apply` tag.  This
illustrates the power of a simple concept like `apply`.

### Using Bind and Apply

What if, in the above example, we decided that the contents of the
header div should be different for different pages?  To do this, we
need a way to pass multiple parameters into a template.  Heist
provides this capability with the `<bind>` tag.  Inside the body of a
`<apply>` tag, you can have multiple bind tags surrounding data to be
passed as separate parameters.  Each `<bind>` tag must have a `tag`
attribute that provides a name for its contents just as described
above.  Then, inside the template, those tags will be substituted with
the appropriate data.

The previous example only needs a few modifications to `default.tpl`
to allow multiple parameters.

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
      <main/>
    </div>
    <div id="footer">
      <p>Copyright XYZ Inc</p>
    </div>
  </body>
</html>
~~~~~~~~~~~~~~~


And `home.tpl` uses the `<bind>` tag with a name attribute to define
values for the `<header/>` and `<main/>` tags:

~~~~~~~~~~~~~~~ {.html}
<apply template="default">
  <bind tag="header">
    <h1>XYZ Inc.</h1>
  </bind>
  Some in-between text.
  <bind tag="main">
    <h1>Home Page</h1>
    <p>Welcome to XYZ Inc</p>
  </bind>
</apply>
~~~~~~~~~~~~~~~

The result template for this example is the same as the previous
example.

NOTE: In this example the `<content/>` tag is still bound as described
above.  The `<content/>` tag is always bound to the complete contents
of the calling `apply` tag.  However, any `bind` tags inside the apply
will disappear.  If we changed `default.tpl` to the following:

~~~~~~~~~~~~~~~ {.html}
<foo>
  <content/>
</foo>
~~~~~~~~~~~~~~~

Then the above `home.tpl` template would render like this:

~~~~~~~~~~~~~~~ {.html}
<foo>
  Some in-between text.
</foo>
~~~~~~~~~~~~~~~


### The `<ignore>` tag

In some cases you may want to include example data in a Heist template
that should not be rendered when the site is active.  Heist provides
the `<ignore>` tag for this purpose.  All `<ignore>` tags and their
contents will be eliminated in a template's output.


### The `<children>` tag

XML requires that well-formed documents have a single root element.
Sometimes you might want to make templates that don't have a single
root element.  In these situations the `<children>` tag is just what
you want.  When the children tag is rendered, it strips itself off and
just returns its child nodes.  This allows you to have a single root
element where necessary, but have that tag disappear in the rendered
output.


