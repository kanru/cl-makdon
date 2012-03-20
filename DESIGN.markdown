CL-MAKDON Design
================

Any Markdown format syntax question should consult the official
definition: <http://daringfireball.net/projects/markdown/syntax>

Pass 1
------

Convert the source to a format that is easier to process.

1.  Convert Setext-style header elements to Atx-style header elements
    representation. ex:

        H1 Header
        =========
        
        H2 Header
        ---------

    become

        (line :text "# H1 Header" :indent 0)
        (line :text "## H2 Header" :indent 0)

2.  Connect multiple-line block elements to a single line
    representation. ex:

        > quote:
        > line one
        > line two

    become

        (line :text "> quote: line one line two" :indent 0)

    This process should aware the i18n issues.
