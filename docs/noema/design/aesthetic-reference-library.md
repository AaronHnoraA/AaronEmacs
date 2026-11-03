# Noema visual reference library

This is the design reference for the Noema Markdown surface and the default
academic PDF. It records sources, the particular visual mechanisms worth
studying, and where those mechanisms belong. Consult it alongside
[Wiki workspace](wiki-workspace.md) for CSS and
[LaTeX export style](latex-export-style.md) for PDF work.

## Vintage scientific pages

[Foadsf/vintage-latex](https://github.com/Foadsf/vintage-latex), reviewed at
`559011918849a3da819912a7c26493071d542df5`, is a gallery of independent
LaTeX examples, not a single drop-in document template. Its
[gallery](https://github.com/Foadsf/vintage-latex#gallery) is the visual index;
the individual examples are the reference when refining a component.

| Example | Mechanism to study | Noema application |
| --- | --- | --- |
| [01 early modern page](https://github.com/Foadsf/vintage-latex/blob/main/examples/01-early-modern-page.tex) | Quiet title, spaced small capitals, opening initial, generous measure, fine printer's rule | Markdown article opener and default PDF title/opening paragraph |
| [04 ornamented incipit](https://github.com/Foadsf/vintage-latex/blob/main/examples/04-ornamented-incipit.tex) | Initial and centered vector ornament as a restrained focal point | Title rule and Latin opening initial; no forced initial for Chinese prose |
| [05 Victorian newspaper](https://github.com/Foadsf/vintage-latex/blob/main/examples/05-victorian-newspaper.tex) | Masthead hierarchy, double rules, metadata strip, column rule | Metadata/title framing and section hierarchy; columns remain an optional layout for short prose |
| [06 natural history catalogue](https://github.com/Foadsf/vintage-latex/blob/main/examples/06-natural-history-catalogue.tex) | Small-cap labels, aligned numeric data, hanging entries, top and bottom rules | Table ruling and aligned figures, compact metadata, theorem labels |
| [13 cooling curve](https://github.com/Foadsf/vintage-latex/blob/main/examples/13-cooling-curve-graph-paper.tex) | Figure, caption and marginal remark as one readable unit | A light separator and measured caption spacing; mathematical figures keep full width |
| [18 mechanics primer](https://github.com/Foadsf/vintage-latex/blob/main/examples/18-fiziko-mechanics-primer.tex) | Figure rhythm and column divider | Future optional illustrated essay layout; default research PDF remains one column for equations, tables and annotations |

Apply a mechanism only where it improves reading. The current shared motif is
hairline rules, centered or well-aligned title groups, small-cap labels,
opening initials, and clear separation between a figure and its caption.
Keep ornaments subordinate to the author's content and preserve semantic
heading, table, figure and theorem structure.

## Product constraints

- Day and Night CSS keep their existing backgrounds, palette tokens and font
  families. Layout, spacing, line weight, letter spacing and ornamentation are
  the adjustable parts. The two themes must remain readable independently.
- The default PDF remains white and suitable for printing or distribution.
  English text uses Libertinus with a portable fallback. Chinese follows the
  Noema Markdown face when installed locally, with Fandol on other machines;
  mathematics stays in Computer Modern/AMS. XeLaTeX is the default engine.
- The PDF must compile on a standard Overleaf XeLaTeX setup. Optional
  typography packages have plain fallbacks. A decorative feature must never
  make a note with Chinese, mathematics, figures or annotations fail to export.
- Test layout with a real long note, a short Chinese note, headings, tables,
  a figure/caption and theorem/proof before accepting a visual change. Inspect
  rendered PDF pages, not only compiler success.

The upstream examples carry
[CC BY-SA 4.0](https://github.com/Foadsf/vintage-latex/blob/main/LICENSE);
their fiziko examples also use an external dependency with its own license.
This catalog stores links and observations. Noema's CSS and LaTeX components
are independently written from these visual principles; upstream source and
preview images are not vendored here.
