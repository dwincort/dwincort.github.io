# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal website (https://danwc.com) built with [Hakyll](https://jaspervdj.be/hakyll/), a Haskell static site generator. The site generator itself is `site.hs`; content lives in Markdown files at the repo root and in `posts/`.

## Branch model — important

- **`hakyll` is the source branch and the default branch.** All work happens here.
- **`master` holds only the generated site** and is written to exclusively by CI. Never commit to or modify `master` directly.
- On every push, CircleCI (`.circleci/config.yml`) builds the site; on pushes to `hakyll`, it additionally commits the generated `_site/` contents to `master`, which GitHub Pages serves.

## Commands

Build the generator and the site with stack (resolver lts-13.6, hakyll 4.12):

```sh
stack build                # compile site.hs into the `site` executable
stack exec site build      # generate the static site into _site/
stack exec site watch      # dev server with live rebuilds (http://localhost:8000)
stack exec site clean      # remove _site/ and _cache/
stack exec site rebuild    # clean + build
```

There are no tests or linters. `_site/` and `_cache/` are gitignored build outputs.

## Architecture

`site.hs` declares all build rules:

- **Content pages**: top-level `.md` files (`projects.md`, `teaching.md`, `personal.md`, `contact.md`, `research.md`) are rendered through `templates/default.html`. `research.md` additionally goes through `templates/bib.html`. `index.md` and `404.md` keep a plain `.html` extension.
- **Posts**: `posts/YYYY-MM-DD-title.md` (date comes from the filename) render through `templates/post.html`, and a snapshot of each post's content feeds the generated `atom.xml` and `rss.xml` feeds (10 most recent). `posts.md` becomes the archive listing via `templates/archive.html`.
- **Clean URLs**: most pages use `cleanRoute`, which routes `foo.md` to `foo/index.html`, and `cleanIndexUrls` strips the trailing `index.html` from internal links. New page rules should follow this pattern.
- **New top-level pages** must be added to the `topLevelPages` pattern in `site.hs` (which also feeds `sitemap.xml`).
- **Static passthrough**: `images/`, `data/`, `css/` (compressed), `robots.txt`, `CNAME`.
- Site root URL is the `root` constant in `site.hs` (`https://danwc.com`).
