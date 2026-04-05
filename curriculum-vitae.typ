// Copyright (c) 2026 Mickaël Canouil
// SPDX-License-Identifier: MIT
//
// Curriculum Vitae — Mickaël Canouil
// https://mickael.canouil.fr
// https://github.com/mcanouil
//

#let updated-at = datetime(year: 2026, month: 4, day: 3)

// Profile picture — set to none to hide, or a path string to show
#let profile-picture = "profile.png"

#set document(
  title: [Curriculum Vitae — Mickaël Canouil],
  author: "Mickaël Canouil",
  date: updated-at,
)
#set page(paper: "a4", margin: 0pt)
#let base-size = 10pt
#let stacked-dates = false
#set text(font: "Georgia", size: base-size, fill: rgb("#1c1c1c"), lang: "en")
#set par(leading: 5.5pt)

// ── Font size scale ─────────────────────────────────────────────────────────
// Change `base-size` above to scale the entire typographic hierarchy.
#let fs = (
  fine: base-size - 3pt, //  7pt — copyright
  date: base-size - 2pt, //  8pt — date ranges
  small: base-size - 2pt, //  8pt — contact, labels, DOI, footer
  label: base-size - 1pt, //  9pt — divider, separator, skills, talks
  body-sm: base-size - 1pt, //  9pt — org/place, client desc, pub body
  body: base-size, // 10pt — entry title (= document default)
  lead: base-size, // 10pt — subtitle
  profile: base-size + 2pt, // 12pt — profile, subtitle
  credential: base-size + 4pt, // 14pt — Ph.D. suffix
  icon: base-size + 6pt, // 16pt — interest icons
  signature: base-size + 14pt, // 24pt — signature
  name: base-size + 24pt, // 34pt — name heading
)

// ── Colour palettes ──────────────────────────────────────────────────────────
// Change `palette` to switch the entire colour scheme.
// Options: "gold", "mcanouil"
#let palette = "gold"

#let palettes = (
  gold: (
    ink: rgb("#111827"),
    accent: rgb("#b5830a"),
    muted: rgb("#6b7280"),
    rule: rgb("#d1c4a0"),
  ),
  mcanouil: (
    ink: rgb("#333333"),
    accent: rgb("#999999"),
    muted: rgb("#888888"),
    rule: rgb("#cccccc"),
  ),
)

#let ink = palettes.at(palette).ink
#let accent = palettes.at(palette).accent
#let muted = palettes.at(palette).muted
#let rule-c = palettes.at(palette).rule

// Date range — renders inline or stacked depending on `stacked-dates`
#let date-range(start, end, stacked: stacked-dates) = {
  if stacked {
    set text(fill: muted, size: fs.date)
    start
    linebreak()
    [#sym.dash.en #end]
  } else {
    text(fill: muted, size: fs.label)[#start -- #end]
  }
}

// Icon helpers — Font Awesome 7
#let fab(code) = text(font: "Font Awesome 7 Brands", code)
#let fas(code) = text(font: "Font Awesome 7 Free Solid", code)

// Contact separator
#let sep = h(1fr) + text(fill: accent.lighten(30%), size: fs.label)[ · ] + h(1fr)

// ── Header ────────────────────────────────────────────────────────────────────
#block(width: 100%, fill: ink)[
  #pad(x: 42pt, top: 18pt, bottom: 12pt)[
    // Name + subtitle block (photo spans this)
    #layout(size => {
      let name-block = [
        #text(size: fs.name, fill: white, tracking: -0.5pt)[
          #text(weight: "light")[MICKAËL ] #text(weight: "bold")[CANOUIL]#text(
            size: fs.credential,
            fill: accent,
            style: "italic",
          )[, Ph.D.]
        ]
        #v(2pt)
        #text(
          size: fs.lead,
          fill: accent,
          tracking: if profile-picture != none { 0.5pt } else { 1pt },
        )[
          BIOSTATISTICIAN #h(1fr) · #h(1fr) QUARTO & R DEVELOPER #h(1fr) · #h(1fr) OPEN-SOURCE CONTRIBUTOR
        ]
      ]
      let block-height = measure(name-block, width: size.width).height
      let photo-size = block-height

      if profile-picture != none {
        grid(
          columns: (auto, 1fr),
          column-gutter: 16pt,
          align: horizon,
          box(
            clip: true,
            radius: 50%,
            width: photo-size,
            height: photo-size,
            stroke: 1.5pt + accent,
          )[#image(
            profile-picture,
            width: photo-size,
            height: photo-size,
            fit: "cover",
            alt: "Profile photo of Mickaël Canouil",
          )],
          name-block,
        )
      } else {
        name-block
      }
    })
    // Contact links (full width, below the photo)
    #v(6pt)
    #line(length: 100%, stroke: 0.4pt + white.transparentize(70%))
    #v(5pt)
    #set text(size: fs.small, fill: white.transparentize(20%))
    #link("mailto:pro@mickael.canouil.dev")[#fas("\u{f0e0}") #h(3pt) pro\@mickael.canouil.dev]
    #sep
    #link("https://mickael.canouil.fr")[#fas("\u{f0ac}") #h(3pt) mickael.canouil.fr]
    #sep
    #link("https://github.com/mcanouil")[#fab("\u{f09b}") #h(3pt) mcanouil]
    #sep
    #link("https://www.linkedin.com/in/mickaelcanouil")[#fab("\u{f08c}") #h(3pt) mickaelcanouil]
    #sep
    #link("https://orcid.org/0000-0002-3396-4549")[#fab("\u{f8d2}") #h(3pt) 0000-0002-3396-4549]
    #sep
    #fas("\u{f3c5}") #h(3pt) Lille, France
  ]
]
#block(width: 100%, height: 3pt, fill: accent)

#block(height: 0pt, clip: true, spacing: 0pt)[#heading(level: 1)[Mickaël Canouil]]

// ── Helpers ───────────────────────────────────────────────────────────────────

// Suppress default heading styling — we handle it in the divider
#show heading: it => none

#let divider(title, level: 2) = {
  v(4pt)
  heading(level: level)[#title]
  grid(
    columns: (auto, 1fr),
    column-gutter: 10pt,
    align: bottom,
    text(size: fs.label, weight: "bold", fill: accent, tracking: 2pt)[#upper(title)],
    line(length: 100%, stroke: 0.8pt + rule-c),
  )
  v(8pt)
}

// Job / education entry — company & place on a separate line
#let entry(title, org, place, start, end, note: none, stacked: stacked-dates) = {
  grid(
    columns: (1fr, auto),
    column-gutter: 6pt,
    text(weight: "bold", size: fs.body)[#title], align(top + right)[#date-range(start, end, stacked: stacked)],
  )
  v(-5pt)
  text(size: fs.body-sm)[#text(fill: accent)[#org] · #text(fill: muted)[#place]]
  if note != none {
    v(2pt)
    {
      set par(justify: true)
      set text(size: fs.body-sm, fill: muted, style: "italic", hyphenate: false)
      note
    }
  }
  v(7pt)
}

// Client sub-entry with date range (stacked by default)
#let client(name, start, end, desc, stacked: true, name-width: auto) = {
  grid(
    columns: (name-width, 1fr, auto),
    column-gutter: 5pt,
    text(fill: accent, weight: "bold", style: "normal")[#name],
    {
      set par(justify: true)
      set text(hyphenate: false)
      desc
    },
    align(top + right)[
      #set text(style: "normal")
      #date-range(start, end, stacked: stacked)
    ],
  )
  v(4pt)
}

// Role sub-entry with date range (stacked by default)
#let subentry(role, employer, start, end, stacked: true) = {
  grid(
    columns: (1fr, auto),
    column-gutter: 5pt,
    [#text(style: "normal", weight: "semibold")[#role] #sym.dash.en #employer],
    align(top + right)[
      #set text(style: "normal")
      #date-range(start, end, stacked: stacked)
    ],
  )
  v(2pt)
}

// Skill category with accent square marker
#let skill-cat(label, items) = {
  grid(
    columns: (7pt, 1fr),
    column-gutter: 4pt,
    text(fill: accent, size: fs.label)[■], text(size: fs.small, weight: "bold", tracking: 0.5pt)[#upper(label)],
  )
  v(2pt)
  pad(left: 11pt)[
    #set text(size: fs.label, fill: ink)
    #items
  ]
  v(6pt)
}

// Open source subsection label
#let sub(label) = {
  text(size: fs.small, weight: "bold", fill: muted, tracking: 0.5pt)[#upper(label)]
  v(4pt)
}

// Talk / workshop item
#let item(title, url, date) = {
  set text(size: fs.label)
  link(url)[#title]
  text(fill: muted)[, #date]
  linebreak()
}

// Publication entry
#let pub(authors, title, journal, year, doi, role: none) = {
  set text(size: fs.body-sm)
  set par(leading: 4pt)
  authors
  [ "#title." ]
  emph(journal)
  [, #year.]
  if role != none {
    text(size: fs.small, fill: muted)[ (#role)]
  }
  h(4pt)
  link("https://doi.org/" + doi)[
    #text(size: fs.small, fill: accent)[doi:#doi]
  ]
  v(5pt)
}

// ── Content area ──────────────────────────────────────────────────────────────
#pad(x: 42pt, top: 20pt, bottom: 38pt)[
  // Subtle dashed underline on links — scoped to content area only
  #show link: it => underline(stroke: (paint: accent.lighten(60%), thickness: 0.4pt, dash: "dashed"), offset: 2pt, it)

  // Profile
  #text(size: fs.profile, style: "italic")[
    #set par(justify: true)
    #set text(hyphenate: false)
    I build tools that make reproducible research easier for data scientists and researchers.
    With a Ph.D. in biostatistics and deep expertise in the Quarto and R ecosystems,
    I bridge the gap between statistical rigour and practical usability.
    49 peer-reviewed papers; 30+ Quarto extensions; creator of the Quarto Extensions Directory and Quarto Wizard.
    Lifelong learner, passionate about sharing knowledge through workshops, open source, and community engagement.
  ]

  #v(6pt)

  // ── Two-column body ──────────────────────────────────────────────────────────
  #grid(
    columns: (1fr, 170pt),
    column-gutter: 28pt,

    // Left column — Experience + Education
    [
      #divider("Professional Experience")

      // ── Astek ──────────────────────────────────────────────────────────────
      #entry(
        "Senior Biostatistician",
        [Astek #sym.dash.en IT&M Stats / Alsinova CRO],
        "Remote",
        [Nov. 2022],
        [present],
      )
      #pad(bottom: 4pt)[
        #set text(size: fs.body-sm, fill: muted, style: "italic")
        #let cw = 55pt
        #client(
          "Client 1", // "L'Oréal",
          [Nov. 2022],
          [Mar. 2026],
          name-width: cw,
        )[
          #text(weight: "semibold")[
            Production-grade R packages] and #text(weight: "semibold")[end-to-end statistical pipelines for multi-omics data] (microbiome, proteomics, lipidomics) and large-scale cohort studies. #text(weight: "semibold")[Fully integrated GitHub / Google Cloud infrastructure]: GitHub Actions for CI/CD triggering, Cloud Build for GCP-side execution, Cloud Workstation as the reproducible development environment, and BigQuery for petabyte-scale data storage and querying.
        ]
        #client(
          "Client 2", // "Servier",
          [Nov. 2025],
          [present],
          name-width: cw,
        )[
          #text(weight: "semibold")[
            All-in-one R package for methylation sequencing]: end-to-end pipeline orchestrated with `targets` (import, quality control, filtering, statistical analysis), #text(weight: "semibold")[interactive `shiny` interface] enabling researchers to import, QC, filter, analyse, and browse methylation data, and #text(weight: "semibold")[automated Quarto CLI reports] generating QC and statistical reports with contextual interpretation guidelines.
        ]
        #client(
          [Consulting \ & Training],
          [Nov. 2022],
          [present],
          name-width: cw,
        )[
          Statistical consulting, R and Quarto training, and workshop delivery for academic and industry clients.
        ]
      ]

      // ── EGID ───────────────────────────────────────────────────────────────
      #block(breakable: false)[
        #entry(
          "Biostatistician, then Head of Biostatistics",
          [CNRS UMR 8199 / Inserm U1283 #sym.dash.en EGID],
          "Lille, France",
          [Sep. 2012],
          [Oct. 2022],
          note: [
            Led multi-omics research in type 2 diabetes genetics: genome-wide and epigenome-wide association studies on international cohorts, eQTL mapping in human pancreatic islets, and joint modelling of longitudinal and survival data. 8 first/co-first/co-last author papers across Nature Communications, Diabetes Care, Diabetes, Bioinformatics, and more. Managed a biostatistics team and coordinated with international consortia: CKDgen, CHARGE, IMIDIA, DIRECT, RHAPSODY.
          ],
        )
        #pad(bottom: 4pt)[
          #set text(size: fs.body-sm, fill: muted, style: "italic")
          #subentry("Head of Biostatistics", "Institut Pasteur de Lille", [Oct. 2017], [Oct. 2022])
          #subentry("Biostatistician", "CNRS", [Sep. 2012], [Sep. 2017])
        ]
      ]

      #block(breakable: false)[
        #divider("Education")

        #entry(
          "Ph.D. in Biostatistics",
          "University of Lille",
          "Lille, France",
          [Oct. 2014],
          [Sep. 2017],
          note: [Thesis: "Joint Modelling of Longitudinal and Survival Data Applied to Genetic Association Studies."],
          stacked: true,
        )
      ]

      #entry(
        "M.Sc. in Biostatistics, Bioinformatics & Genomics",
        "University Claude Bernard Lyon 1",
        "Lyon, France",
        [Sep. 2009],
        [Jul. 2011],
        note: "Specialised in Biostatistics.",
        stacked: true,
      )

      #entry(
        "B.Sc. in Biology",
        "University Claude Bernard Lyon 1",
        "Lyon, France",
        [Sep. 2006],
        [Jul. 2009],
        note: "Specialised in Biostatistics.",
        stacked: true,
      )
    ],

    // Right column — Skills, Open Source, Talks
    [
      #divider("Technical Skills")

      #skill-cat("Statistics & Methods")[
        GWAS, multi-omics (proteomics, metabolomics, transcriptomics, epigenomics), mixed & joint models, survival analysis, meta-analysis
      ]
      #skill-cat("Infrastructure")[
        GitHub (Actions, Pages), Docker, Dev Containers, Google Cloud (Workstation, BigQuery, Cloud Build), VSCode/Positron extensions
      ]
      #skill-cat("Dev Languages")[
        R, Lua, Typst, Python, TypeScript, JavaScript, Bash, SQL
      ]
      #skill-cat("R Ecosystem")[
        Tidyverse (ggplot2, shiny, ...), devtools (testthat, roxygen2, ...), Bioconductor (limma, DESeq2, ...), renv, targets
      ]
      #skill-cat("Authoring & Publishing")[
        Quarto, Typst, LaTeX, Reveal.js, Pandoc, R Markdown, HTML/(S)CSS
      ]

      #set text(size: fs.body-sm)

      #block(breakable: false)[
        #divider("Open Source")
        #sub("Quarto & Typst")
        #link("https://m.canouil.dev/quarto-extensions/")[_Quarto Extensions Directory_] \
        #link("https://github.com/mcanouil/quarto-wizard")[_Quarto Wizard_] #sym.dash.en VSCode & Positron \
        30+ extensions: Lua filters/shortcodes, Typst/HTML/Reveal.js formats, _etc._
      ]

      #v(6pt)
      #block(breakable: false)[
        #sub("R Packages")
        #link("https://github.com/mcanouil/NACHO")[_NACHO_], #link("https://github.com/mcanouil/insane")[_insane_], #link("https://github.com/mcanouil/MiSTr")[_MiSTr_], #link("https://github.com/mcanouil/snpEnrichment")[_snpEnrichment_], #link("https://github.com/mcanouil/ggpacman")[_ggpacman_] and more #sym.dash.en spanning genomics QC, statistical testing, and data visualisation.
      ]

      #v(4pt)
      #block(breakable: false)[
        #divider("Talks")

        #item(
          "Introduction to Development Containers",
          "https://mickael.canouil.fr/talks/2025-10-08-devcontainer-introduction/",
          "Oct. 2025",
        )
        #item(
          "A Gentle Introduction to Quarto",
          "https://mickael.canouil.fr/talks/2023-10-19-gentle-intro-to-quarto/",
          "Oct. 2023",
        )
        #item(
          "My Journey to Transparency and Reproducibility",
          "https://mickael.canouil.fr/talks/2021-04-15-rlille-reproducibility/",
          "Apr. 2021",
        )
        #item(
          "A Statistical Seminar Applied on Type 2 Diabetes",
          "https://mickael.canouil.fr/talks/2019-01-21-lund-sweden/",
          "Jan. 2019",
        )
      ]

      #v(4pt)
      #block(breakable: false)[
        #divider("Workshops")

        #item(
          "Mastering Quarto CLI",
          "https://github.com/mcanouil/mastering-quarto-cli",
          "Feb. 2026",
        )
        #item(
          "R Advanced",
          "https://github.com/mcanouil/radvanced",
          "2020, 2021",
        )
        #item(
          "Shiny: First Steps",
          "https://github.com/mcanouil/rshiny",
          "2019, 2021",
        )
        #item(
          "R Packages",
          "https://github.com/mcanouil/rpackages",
          "Feb. 2020",
        )
      ]

      #v(4pt)
      #text(size: fs.small)[
        #text(fill: accent, weight: "bold")[Languages:]
        French (native) · English (fluent)
      ]
    ],
  )

  // ── Selected Publications (full width) ──────────────────────────────────────
  #block(breakable: false)[
    #divider("Selected Publications")

    // 2024 — co-last (pos 19/20)
    #pub(
      [Burrows K., ..., *Canouil M.*#super[†], Warrington N.M.],
      [A Framework for Conducting GWAS Using Repeated Measures Data with an Application to Childhood BMI],
      [Nature Communications],
      [2024],
      "10.1038/s41467-024-53687-3",
      role: "†co-last",
    )
  ]
  // 2021 — first (pos 1/15)
  #pub(
    [*Canouil M.*, Khamis A., _et al._],
    [Epigenome-Wide Association Study Reveals Methylation Loci Associated With Offspring Gestational Diabetes Mellitus Exposure and Maternal Methylome],
    [Diabetes Care],
    [2021],
    "10.2337/dc20-2960",
  )
  // 2020 — first (pos 1/6)
  #pub(
    [*Canouil M.*, Bouland G.A., Bonnefond A., Froguel P., 't Hart L.M., Slieker R.C.],
    [NACHO: An R Package for Quality Control of NanoString nCounter Data],
    [Bioinformatics],
    [2020],
    "10.1093/bioinformatics/btz647",
  )
  // 2019 — co-first (pos 2/20)
  #pub(
    [Khamis A., *Canouil M.*#super[†], _et al._],
    [Laser Capture Microdissection of Human Pancreatic Islets Reveals Novel eQTLs Associated with Type 2 Diabetes],
    [Molecular Metabolism],
    [2019],
    "10.1016/j.molmet.2019.03.004",
    role: "†co-first",
  )
  // 2018 — first (pos 1/5)
  #pub(
    [*Canouil M.*, Balkau B., Roussel R., Froguel P., Rocheleau G.],
    [Jointly Modelling Single Nucleotide Polymorphisms With Longitudinal and Time-to-Event Trait: An Application to Type 2 Diabetes and Fasting Plasma Glucose],
    [Frontiers in Genetics],
    [2018],
    "10.3389/fgene.2018.00210",
  )
  // 2018 — co-first (pos 4/38)
  #pub(
    [Abderrahmani A., ..., *Canouil M.*#super[†], _et al._],
    [Increased Hepatic PDGF-AA Signaling Mediates Liver Insulin Resistance in Obesity-Associated Type 2 Diabetes],
    [Diabetes],
    [2018],
    "10.2337/db17-1539",
    role: "†co-first",
  )
  // 2017 — co-first (pos 2/8)
  #pub(
    [Verbanck M., *Canouil M.*#super[†], _et al._],
    [Low-Dose Exposure to Bisphenols A, F and S of Human Primary Adipocyte Impacts Coding and Non-Coding RNA Profiles],
    [PLOS ONE],
    [2017],
    "10.1371/journal.pone.0179583",
    role: "†co-first",
  )
  // 2017 — co-first (pos 3/22)
  #pub(
    [Ndiaye F.K., ..., *Canouil M.*#super[†], _et al._],
    [Expression and Functional Assessment of Candidate Type 2 Diabetes Susceptibility Genes Identify Four New Genes Contributing to Human Insulin Secretion],
    [Molecular Metabolism],
    [2017],
    "10.1016/j.molmet.2017.03.011",
    role: "†co-first",
  )
  #text(size: fs.label, fill: muted)[
    #text(fill: accent, weight: "bold")[49] peer-reviewed papers in total, including publications in _Nature_, _Nature Genetics_, and _Diabetes_.
  ]

  #v(6pt)

  // ── Interests (full width) ──────────────────────────────────────────────────
  #divider("Interests")

  #set text(size: fs.body-sm)
  #grid(
    columns: (1fr, 1fr, 1fr),
    column-gutter: 16pt,
    align: center,
    [
      #text(fill: accent, size: fs.icon, baseline: fs.icon / 6)[#fas("\u{f008}")] #h(6pt)
      #text(weight: "bold")[Cinephile] \
      #v(2pt)
      #text(size: fs.body-sm, fill: muted)[4,000 films,\ half on the big screen.]
    ],
    [
      #text(fill: accent, size: fs.icon, baseline: fs.icon / 6)[#fas("\u{f522}")] #h(6pt)
      #text(weight: "bold")[Board games] \
      #v(2pt)
      #text(size: fs.body-sm, fill: muted)[From strategy to party,\ no genre refused.]
    ],
    [
      #text(fill: accent, size: fs.icon, baseline: fs.icon / 6)[#fas("\u{f1b0}")] #h(6pt)
      #text(weight: "bold")[Saga] #text(fill: accent)[#sym.mars] \
      #v(2pt)
      #text(size: fs.body-sm, fill: muted)[Black Labrador and\ walking companion.]
    ],
  )

  #v(1fr)
  #h(1fr)
  #text(font: "Snell Roundhand", size: fs.signature, fill: accent, weight: "bold")[Mickaël Canouil]
  #h(0.1fr)
  #v(1fr)
  #align(center)[
    #text(
      size: fs.fine,
      fill: muted,
    )[© #updated-at.display("[year]") Mickaël Canouil · #link("https://creativecommons.org/licenses/by-nc-nd/4.0/")[CC BY-NC-ND 4.0]]
  ]
]

// ── Footer ────────────────────────────────────────────────────────────────────
#place(bottom + left)[
  #block(width: 100%)[
    #block(width: 100%, height: 3pt, fill: accent)
    #block(width: 100%, fill: ink)[
      #pad(x: 42pt, y: 6pt)[
        #set text(size: fs.small)
        #text(fill: white.transparentize(30%))[Updated: #updated-at.display("[month repr:short]. [year]")]
        #h(1fr)
        #text(fill: accent, weight: "bold", tracking: -0.3pt)[MICKAËL CANOUIL]
      ]
    ]
  ]
]
