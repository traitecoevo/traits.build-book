# Contributing

Development is a community effort, and we welcome participation.

## Code of Conduct

While this project is not part of ropensci, we will adopt their [code of conduct](https://ropensci.org/code-of-conduct/). Please read it before engaging in discussion.

## Discussions

At <https://github.com/traitecoevo/traits.build-book/discussions>, you can post general questions, brainstorm ideas, and ask for help.

## Issues

<https://github.com/traitecoevo/traits.build-book/issues> is for bug reports, performance issues, maintenance tasks, and feature requests. When you post, please abide by the following guidelines.

* Before posting a new issue, please take a moment to search for existing similar issues in order to avoid duplication.
* For bug reports: if you can, please install the latest GitHub version of `traits.build` (i.e. `remotes::install_github("traitecoevo/traits.build")`) and verify that the issue still persists.
* Describe your issue in prose as clearly and concisely as possible.
* Include diagnostic details about the problem, including
    * A [reproducible example](https://github.com/tidyverse/reprex).
    * Session info, available through `sessionInfo()` or [`reprex(si = TRUE)`](https://github.com/tidyverse/reprex).
    * A stack trace from `traceback()` or `rlang::trace_back()`.
    * The [SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the GitHub commit of `traits.build` currently installed. `packageDescription("traits.build")$GithubSHA1` shows you this.
    
## Development

External code contributions are extremely helpful in the right circumstances. Here are the recommended steps.

1. Prior to contribution, please propose your idea in a [new issue thread](https://github.com/traitecoevo/traits.build-book/issues) so you and the maintainer can define the intent and scope of your work.
2. [Fork the repository](https://help.github.com/articles/fork-a-repo/).
3. Follow the [GitHub flow](https://guides.github.com/introduction/flow/index.html) to create a new branch, add commits, and open a pull request.
4. Discuss your code with the maintainer in the pull request thread.
5. If everything looks good, the maintainer will merge your code into the project.

Please also follow these additional guidelines.

* Respect the architecture and reasoning of the project.
* If possible, keep contributions small enough to easily review manually. It is okay to split up your work into multiple pull requests.
* Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) and check your formatting with the `lint_package()` function from the [`lintr`](https://github.com/jimhester/lintr) package.
* Describe your contribution in the project's [`NEWS.md`](https://github.com/traitecoevo/traits.build-book/blob/main/NEWS.md) file. Be sure to mention relevent GitHub issue numbers and your GitHub name as done in existing news entries.
* If you feel contribution is substantial enough for official author or contributor status, please add yourself as an author in the [`DESCRIPTION](https://github.com/traitecoevo/traits.build-book/blob/main/DESCRIPTION) file.



## Acknowledgements and copyright

This page was adapted from a corresponding file for the `targets`` package, with text by Will Landau. The original file is available at <https://github.com/ropensci-books/targets/>. Text adapted from that page is under copy right specified in that package <https://github.com/ropensci-books/targets/blob/main/LICENSE.md>.

