Wisconsin K12 School Data
================
Package maintained by Spencer Schien –
Last updated 2022-01-19

# Introduction

The `wisconsink12` package was developed to increase the accessibility
of publicly available data regarding K12 schools in Wisconsin and to
facilitate analysis of that data.

> :star: *Data is also written to a SQLite database, which which can be
> provided upon request. Data dictionaries can be accessed outside of R
> in this
> [repository](https://github.com/cityforwardcollective/wi_schools).*

The data is organized in a relational database structure, where each
table has a unique school identifier that facilitates joins. The
following data tables are included:

-   `act` – This table contains ACT results for public and
    Choice-participating private schools. Disaggregated test results are
    not available for private schools.
-   `attendance`
-   `choice_counts` – This table contains counts of students
    participating in the Milwaukee, Racine, and Wisconsin Parental
    Choice Programs, as well as the Special Needs Scholarship Program.  
-   `discipline`
-   `enrollment` – This table contains enrollment data for all Wisconsin
    schools. Disaggregated enrollment data is not available for private
    schools.
-   `forward_exam` – This table contains Forward Exam results for public
    and Choice-participating private schools. Disaggregated test results
    are not available for private schools.
-   `graduation` – This table contains graduation and completion data
    for all public schools. Private schools are not included here at all
    because cohort counts are not available for private schools, and so
    rates cannot be calculated.
-   `other_enrollment` – This table provides counts of students missed
    by other reporting methods (i.e. mobile students, Open Enrollment,
    Chapter 220).
-   `report_cards` – This table contains Report Card data from all
    schools in Wisconsin.
-   `schools` – This is a list of all schools in Wisconsin that serves
    as the unique identifier table for the relational database.

# Installation

The plan is to submit the `wisconsink12` package to CRAN, but for now it
must be downloaded from City Forward Collective’s [GitHub
repository](https://github.com/cityforwardcollective). This can be
achieved with the following code.

``` r
# The remotes package is required
# to download from GitHub.
# install.packages("remotes") if you haven't already

remotes::install_gitub("cityforwardcollective/wisconsink12")
```

# Getting Started

The `wisconsink12` package is built around the school data it makes
available. This data is organized into tables (listed above), and each
table contains a school ID field called the `dpi_true_id`. This field is
a concatenation of a school’s District Code and its School Code, with an
underscore ’\_’ in between. Both codes are padded with zeros on the left
to a length of four, and choice schools are given a District Code of
‘0000’.

Once the `wisconsink12` package is loaded, a message will be displayed
listing the tables that are available.

``` r
# Trying to access tables before loading
# the package will result in an error.

# Load the package, then access the tables.
library(wisconsink12)
#> The following tables are now available:
#> - schools
#> - enrollment
#> - attendance
#> - discipline
#> - retention
#> - report_cards
#> - forward_exam
#> - graduation
#> - choice_counts
#> - other_enrollment
#> - act
```

As the output shows, we indeed have access to the 11 tables listed
above. We can inspect these tables as we would any dataframe.

``` r
# Inspect the `schools` table

str(schools)
#> 'data.frame':    18531 obs. of  25 variables:
#>  $ school_year           : chr  "2015-16" "2015-16" "2015-16" "2015-16" ...
#>  $ dpi_true_id           : chr  "3619_0162" "3619_0413" "3619_1063" "3619_0432" ...
#>  $ school_name           : chr  "ALBA - Academia de Lenguaje y Bellas Artes" "Alliance School of Milwaukee" "ASSATA" "Banner Preparatory School of Milwaukee" ...
#>  $ agency_type           : chr  "Public school" "Public school" "Public school" "Public school" ...
#>  $ district_name         : chr  "Milwaukee" "Milwaukee" "Milwaukee" "Milwaukee" ...
#>  $ cesa                  : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ county                : chr  "Milwaukee" "Milwaukee" "Milwaukee" "Milwaukee" ...
#>  $ charter_indicator     : num  1 1 0 0 1 0 1 1 0 0 ...
#>  $ choice_indicator      : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ accurate_agency_type  : chr  "Instrumentality Charter" "Instrumentality Charter" "Partnership" "Partnership" ...
#>  $ broad_agency_type     : chr  "District Operated" "District Operated" "Independently Operated" "Independently Operated" ...
#>  $ MPCP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ RPCP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ WPCP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ SNSP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ MPCP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ RPCP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ WPCP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ SNSP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ locale_description    : chr  "City" "City" "City" "City" ...
#>  $ city                  : chr  "Milwaukee" "Milwaukee" "Milwaukee" "Milwaukee" ...
#>  $ MPCP_percent          : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ milwaukee_indicator   : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lowest_enrolled_grade : chr  "K3" "9" "9" "9" ...
#>  $ highest_enrolled_grade: chr  "5" "12" "12" "12" ...
#>  - attr(*, "data_dictionary")= spec_tbl_df [15 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#>   ..$ variable           : chr [1:15] "dpi_true_id" "school_name" "agency_type" "district_name" ...
#>   ..$ description        : chr [1:15] "Unique school identifier" "School name" "Agency type as designated by DPI" "District name" ...
#>   ..$ data_transformation: chr [1:15] "Concatenation of the District and School ID designated by DPI.  Private Schools are given the District ID of '0000'." "None" "None" "None" ...
#>   ..$ data_source        : chr [1:15] "All sources" "Enrollment data download files" "Public enrollment data download file (all private schools are marked as Private)" "Public enrollment data download file" ...
#>   ..- attr(*, "spec")=List of 3
#>   .. ..$ cols   :List of 4
#>   .. .. ..$ variable           : list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. .. ..$ description        : list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. .. ..$ data_transformation: list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. .. ..$ data_source        : list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ default: list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
#>   .. ..$ delim  : chr ","
#>   .. ..- attr(*, "class")= chr "col_spec"
#>   ..- attr(*, "problems")=<externalptr>
```

If you get further along after loading the package and you forget what
tables are available, you can access that information with the
`list_tables()` function.

``` r
# List the tables available in the 
# `wisconsink12` package.

list_tables()
#>  [1] "schools"          "enrollment"       "attendance"       "discipline"      
#>  [5] "retention"        "report_cards"     "forward_exam"     "graduation"      
#>  [9] "choice_counts"    "other_enrollment" "act"
```

# Common Use Cases

## Schools

There are a few slices of this data that will be a starting point for an
analysis the vast majority of the time. For example, you might want to
perform an analysis focused on Milwaukee. To easily filter for these
schools, you can use the `make_mke_schools()` function.

*Note: The `wisconsink12` package designates a schools as a Milwaukee
school in filters if the school is located within city limits or if the
school is a private school outside city limits but participates in the
Milwaukee Parental Choice Program (MPCP) with at least 75% of its total
enrollment made up of MPCP-funded students.*

``` r
# `make_mke_schools()` will filter the `schools` table and return only those
# schools designated by the `milwaukee_indicator` column. The returned table 
# will contain observations of all years of data available.

mke_schools <- make_mke_schools()

# Inspect the resulting dataframe
str(mke_schools)
#> 'data.frame':    1623 obs. of  25 variables:
#>  $ school_year           : chr  "2015-16" "2015-16" "2015-16" "2015-16" ...
#>  $ dpi_true_id           : chr  "3619_0162" "3619_0413" "3619_1063" "3619_0432" ...
#>  $ school_name           : chr  "ALBA - Academia de Lenguaje y Bellas Artes" "Alliance School of Milwaukee" "ASSATA" "Banner Preparatory School of Milwaukee" ...
#>  $ agency_type           : chr  "Public school" "Public school" "Public school" "Public school" ...
#>  $ district_name         : chr  "Milwaukee" "Milwaukee" "Milwaukee" "Milwaukee" ...
#>  $ cesa                  : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ county                : chr  "Milwaukee" "Milwaukee" "Milwaukee" "Milwaukee" ...
#>  $ charter_indicator     : num  1 1 0 0 1 0 1 1 0 0 ...
#>  $ choice_indicator      : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ accurate_agency_type  : chr  "Instrumentality Charter" "Instrumentality Charter" "Partnership" "Partnership" ...
#>  $ broad_agency_type     : chr  "District Operated" "District Operated" "Independently Operated" "Independently Operated" ...
#>  $ MPCP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ RPCP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ WPCP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ SNSP_indicator        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ MPCP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ RPCP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ WPCP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ SNSP_has_students     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ locale_description    : chr  "City" "City" "City" "City" ...
#>  $ city                  : chr  "Milwaukee" "Milwaukee" "Milwaukee" "Milwaukee" ...
#>  $ MPCP_percent          : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ milwaukee_indicator   : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lowest_enrolled_grade : chr  "K3" "9" "9" "9" ...
#>  $ highest_enrolled_grade: chr  "5" "12" "12" "12" ...
#>  - attr(*, "data_dictionary")= spec_tbl_df [15 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#>   ..$ variable           : chr [1:15] "dpi_true_id" "school_name" "agency_type" "district_name" ...
#>   ..$ description        : chr [1:15] "Unique school identifier" "School name" "Agency type as designated by DPI" "District name" ...
#>   ..$ data_transformation: chr [1:15] "Concatenation of the District and School ID designated by DPI.  Private Schools are given the District ID of '0000'." "None" "None" "None" ...
#>   ..$ data_source        : chr [1:15] "All sources" "Enrollment data download files" "Public enrollment data download file (all private schools are marked as Private)" "Public enrollment data download file" ...
#>   ..- attr(*, "spec")=List of 3
#>   .. ..$ cols   :List of 4
#>   .. .. ..$ variable           : list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. .. ..$ description        : list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. .. ..$ data_transformation: list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. .. ..$ data_source        : list()
#>   .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ default: list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
#>   .. ..$ delim  : chr ","
#>   .. ..- attr(*, "class")= chr "col_spec"
#>   ..- attr(*, "problems")=<externalptr>
```

The resulting `mke_schools` dataframe produced above will be a list of
Milwaukee schools for each year since the 2015-16 school year (the first
year included in the database). The table will include descriptive
characteristics for the schools such as the school code and name, agency
type (i.e. sector), choice program participation, grades served, etc.

With this table loaded, we can easily calculate the count of Milwaukee
schools per year and per agency type.

``` r
# I'll make use of several `tidyverse` libraries going forward,
# and `kableExtra` for the tables

library(tidyverse)
library(kableExtra)

# Calculate the number of schools in each sector

mke_schools %>%
  group_by(broad_agency_type, accurate_agency_type, school_year) %>%
  summarise(N = n()) %>%
  pivot_wider(names_from = school_year, values_from = N) %>%
  kbl(booktabs = T, caption = "Annual Milwaukee School Counts by Agency Type") 
```

<table>
<caption>
Annual Milwaukee School Counts by Agency Type
</caption>
<thead>
<tr>
<th style="text-align:left;">
broad_agency_type
</th>
<th style="text-align:left;">
accurate_agency_type
</th>
<th style="text-align:right;">
2015-16
</th>
<th style="text-align:right;">
2016-17
</th>
<th style="text-align:right;">
2017-18
</th>
<th style="text-align:right;">
2018-19
</th>
<th style="text-align:right;">
2019-20
</th>
<th style="text-align:right;">
2020-21
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
District Operated
</td>
<td style="text-align:left;">
Instrumentality Charter
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
District Operated
</td>
<td style="text-align:left;">
Traditional Public
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
132
</td>
<td style="text-align:right;">
132
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:right;">
130
</td>
</tr>
<tr>
<td style="text-align:left;">
Independently Operated
</td>
<td style="text-align:left;">
2r/2x Charter
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
Independently Operated
</td>
<td style="text-align:left;">
Non-Instrumentality Charter
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Independently Operated
</td>
<td style="text-align:left;">
Partnership
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Private
</td>
<td style="text-align:left;">
Private
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
92
</td>
<td style="text-align:right;">
92
</td>
<td style="text-align:right;">
90
</td>
<td style="text-align:right;">
90
</td>
</tr>
</tbody>
</table>

In addition to illustrating school counts, this table demonstrates how
the `broad_agency_type` and `accurate_agency_type` variables are
related. As the names suggest, the `broad_agency_type` is a broader
grouping, and the `accurate_agency_type` is the more granular level.

**Note that to delineate between Non-Instrumentality Charters and
Instrumentality Charters, we manually code the Instrumentality Charters
contracted with Milwaukee Public Schools. This means these designations
only work for Milwaukee schools at the moment.**

## Report Cards

Maybe the next most common use case would be to look at Report Card
data. The `wisconsink12` package facilitates this with the
`make_mke_rc()` function. The primary result of this function, a
dataframe of Report Card data filtered for Milwaukee schools. By
default, this function will include the *Private - Choice Students*
report card for private schools. The *Private - All Students* report
card can be selected by setting `private_type = "all"` (the default is
`private_type = "choice`).

``` r
# This will create a dataframe with Report Card data for all Milwaukee schools,
# selecting the Private - Choice Students Report Card for private schools.

mke_rc <- make_mke_rc()

# And this one will select the Private - All Students report card where it is 
# available for a Private school. For schools that don't elect to get the 
# All Students report card, the Choice Students one will be selected.

mke_rc_all <- make_mke_rc(private_type = "all")
```

## Other Data Points

There currently aren’t distinct query functions like `make_mke_rc()` for
each table provided by the `wisconsink12` package. To filter other
tables for Milwaukee schools (or any ad hoc list of schools), you can
use joins with the `schools` table.

For instance, if you want to look at Forward Exam results for Milwaukee
2r/2x Charters, you can use the following code:

``` r
# This will result in a dataframe of only 2r2x Charter schools in Milwaukee

mke_2r2x <- mke_schools %>%
  filter(accurate_agency_type == "2r/2x Charter")

# Next, we'll join this table with the `forward_exam` table, which will
# effectively filter the Forward Exam data while also provided helpful
# variables in the `schools` table

mke_2r2x_forward <- left_join(mke_2r2x, forward_exam)
```

By default, join functions will join on columns that are shared in the
tables. The `wisconsink12` tables are intended to be joined by
`school_year` and `dpi_true_id`, which is how the above code will
function.

# Conclusion

The `wisconsink12` package is intended to be a resource for the public.
In its current iteration, it is admittedly a niche product given that
one needs specialized skills with R to most easily access the data.
Also, the package has been built and maintained by a single person
working in the Milwaukee context. It is therefore entirely possible that
the author has taken for granted certain knowledge or made assumptions
that are not obvious to those who might use this package.

Please provide feedback, submit bug reports, request features, or just
ask questions if you use this package.
