library(shiny)
library(bslib)
library(dplyr)
library(DT)

source_base <- "https://cran.r-project.org/web/packages/"
contrib_base <- "https://repo.r-wasm.org/bin/emscripten/contrib/"
versions <- c("4.2.x" = "4.2", "4.3.x" = "4.3", "4.3.3" = "4.3.3", "4.4.x" = "4.4", "4.5.x" = "4.5")

ui <- page_sidebar(
  title = h1("WebR binary R package repository"),
  sidebar = sidebar(
    title = "Options",
    open = FALSE,
    selectInput("version", "Select R version",
      choices = versions,
      selected = versions[[length(versions)]]
    ),
  ),
  p(
    class = "lead",
    "This CRAN-like repository contains R packages compiled to WebAssembly for use with webR. Set this page's URL as the named",
    code("repos"), "argument when using the", code("install.packages()"),
    "command to use this repository as the source for downloading binary R packages."
  ),
  p(
    "By default, ", code("install.packages()"), "will use the public repository hosted at",
    a(href = "https://repo.r-wasm.org/", "https://repo.r-wasm.org/"),
    ". See the", a(href = "https://docs.r-wasm.org/webr/latest/packages.html", "webR documentation"),
    "for further information about webR."
  ),
  h2("Repository statistics"),
  layout_columns(
    fill = FALSE,
    value_box(
      title = h2("Built R packages"),
      value = textOutput("built"),
      showcase = bsicons::bs_icon("hammer"),
      "Packages that have been built for WebAssembly and are available for download from this repository."
    ),
    value_box(
      title = "Available R packages",
      value = textOutput("available"),
      showcase = bsicons::bs_icon("check-circle"),
      "Packages for which all of the package dependencies have also been built for WebAssembly and are available for download from this repo."
    ),
  ),
  h2("Packages"),
  DTOutput("webr_pkgs")
)

server <- function(input, output) {
  res <- reactive({
    withProgress(
      {
        repo_info <- as.data.frame(available.packages(
          contriburl = paste0(contrib_base, input$version),
          filters = c("OS_type", "subarch", "duplicates")
        ))
        avail_pkgs <- c(
          rownames(repo_info),
          c(
            "base", "compiler", "datasets", "graphics", "grDevices",
            "grid", "methods", "splines", "stats", "stats4",
            "tools", "utils", "parallel", "webr"
          )
        )
        incProgress(2 / 5)

        deps <- tools::package_dependencies(
          packages = rownames(repo_info),
          db = repo_info, recursive = TRUE
        )
        incProgress(2 / 5)

        deps <- tibble(
          Package = names(deps),
          Available = deps |> purrr::map(\(x) all(x %in% avail_pkgs)),
          Depends = deps,
          Missing = deps |> purrr::map(\(x) x[!(x %in% avail_pkgs)]),
        )
        incProgress(1 / 5)

        package_table <- repo_info |>
          select(c("Package", "Version", "Repository")) |>
          left_join(deps, by = "Package") |>
          arrange(Package)

        list(
          table = package_table,
          n_built = dim(package_table)[1],
          n_avail = sum(as.numeric(deps$Available))
        )
      },
      message = "Loading package lists and crunching dependencies",
      detail = "This may take a little while...",
      value = 0
    )
  })

  output$built <- renderText(res()$n_built)
  output$available <- renderText(res()$n_avail)
  output$webr_pkgs <- renderDT(
    datatable(
      res()$table,
      rownames = FALSE,
      selection = "none",
      options = list(
        ordering = FALSE,
        search = list(regex = TRUE),
        columns = JS("[
          null,
          null,
          { searchable: false, visible: false },
          { title: 'All depends available?' },
          {
            searchable: false,
            title: 'Depends<br><small>Missing dependencies are shown in bold.</small>',
          },
          { searchable: false, visible: false },
        ]"),
        rowCallback = JS(paste0(
          "
          function(row, data) {
            if (data[3][0]) {
              $('td:eq(2)', row).html('Yes');
            } else {
              $('td:eq(2)', row).html('<b>No</b>');
            }
            $('td:eq(0)', row).html(
          ",
          if (is.null(source_base)) {
            "data[0]"
          } else {
            paste0("`<a target=\"_blank\" href=\"", source_base, "${data[0]}/\">${data[0]}</a>`")
          },
          "
            );
            $('td:eq(3)', row).html(data[4].map((v) => {
              if (data[5].includes(v))
                return '<b>' + v + '</b>';
              return v;
            }).join(', '));
          }
        "
        ))
      )
    )
  )
}

shinyApp(ui = ui, server = server)
