# URLroute

*URLroute*  aims to be a type-safe URL-dispatching library for Haskell.

## Goals

- Dispatch URL in a type safe manner

- Build URLs from the same routes used for dispatching

- Support URL-canonicalization

- Support showing of routes for reflection, debugging purposes

- A independent from a concrete URL-representation to be able to accommondate
  different web-frameworks

## Example

Consider the following fragment:

    -- URL for a article page
    (buildArticle, dispatchArticle) = root </ "blog" </ string
                                      ==> \article -> ... generate page ...

    -- URL for a comment
    (buildComment, dispatchComment) = root </ "blog" </ string <& "comment" := int
                                      ==> \article commentNo -> ... generate comment ...

    dispatchBlog :: URL -> Page
    dispatchBlog url = dispatch [dispatchArticle, dispatchComment] url

The operator `==>` binds a route with typeful "holes", i.e. `string`, `int`,
with a function taking the matched values.
Moreover a builder function taking values of the same types constructiing
an URL is returned, i.e.

    > buildComment "test" 1
    /blog/test?comment=1


