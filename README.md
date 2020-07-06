
# quanteda.tidy

<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/quanteda.tidy)](https://cran.r-project.org/package=quanteda.tidy)
[![](https://img.shields.io/badge/devel%20version-0.2-royalblue.svg)](https://github.com/quanteda/quanteda.tidy)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/quanteda/quanteda.tidy.svg?branch=master)](https://travis-ci.org/quanteda/quanteda.tidy)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/quanteda/quanteda.tidy?branch=master&svg=true)](https://ci.appveyor.com/project/quanteda/quanteda.tidy)
[![Coverage
Status](https://img.shields.io/codecov/c/github/quanteda/quanteda.tidy/master.svg)](https://codecov.io/github/quanteda/quanteda.tidy?branch=master)
<!-- badges: end -->

## About

**quanteda.tidy** extends the **quanteda** package with functionality
from the “tidyverse”, especially **dplyr**.  
Note that this is not the same as **tidytext**, which stretches tokens
into data.frames. Instead, tidy functions operate only on document
variables, but extends these functions (from **dplyr**) to work on
**quanteda** objects as if they were tibbles or data.frames.

## Installation

You can install **quanteda.tidy** from GitHub with:

``` r
devtools::install_github("quanteda/quanteda.tidy")
```

## Examples

Adding a document variable for full president name:

``` r
library("quanteda.tidy", warn.conflicts = FALSE)
## Loading required package: quanteda
## Package version: 2.0.2
## Parallel computing: 2 of 8 threads used.
## See https://quanteda.io for tutorials and examples.
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View

data_corpus_inaugural %>%
  transmute(fullname = paste(FirstName, President, sep = ", ")) %>%
  summary(n = 5)
## Warning: n argument is not used.
##             doc_id number punct symbol any n_token n_type      noise n_sent
## 1  1789-Washington      1   108      0 109    1537   9360 0.07091737     23
## 2  1793-Washington      0    12      0  12     147   9360 0.08163265      4
## 3       1797-Adams      0   263      0 263    2577   9360 0.10205666     37
## 4   1801-Jefferson      0   203      0 203    1923   9360 0.10556422     41
## 5   1805-Jefferson      0   216      0 216    2380   9360 0.09075630     45
## 6     1809-Madison      0    88      0  88    1261   9360 0.06978588     21
## 7     1813-Madison      0    93      0  93    1302   9360 0.07142857     33
## 8      1817-Monroe      0   315      0 315    3677   9360 0.08566766    121
## 9      1821-Monroe     13   424      2 436    4886   9360 0.08923455    129
## 10      1825-Adams      0   234      0 234    3147   9360 0.07435653     74
## 11    1829-Jackson      0    80      0  80    1208   9360 0.06622517     25
## 12    1833-Jackson      0    92      0  92    1267   9360 0.07261247     29
## 13   1837-VanBuren      0   326      0 326    4158   9360 0.07840308     95
## 14   1841-Harrison      0   702      0 702    9123   9360 0.07694837    210
## 15       1845-Polk      1   394      0 395    5186   9360 0.07616660    153
## 16     1849-Taylor      0    93      0  93    1178   9360 0.07894737     22
## 17     1853-Pierce      1   314      0 315    3636   9360 0.08663366    104
## 18   1857-Buchanan      0   271      0 271    3083   9360 0.08790139     89
## 19    1861-Lincoln      4   370      0 374    3999   9360 0.09352338    135
## 20    1865-Lincoln      0    82      0  82     775   9360 0.10580645     26
## 21      1869-Grant      0   107      0 107    1229   9360 0.08706265     40
## 22      1873-Grant      2   137      0 139    1472   9360 0.09442935     43
## 23      1877-Hayes      2   231      0 232    2707   9360 0.08570373     59
## 24   1881-Garfield      4   250      0 253    3209   9360 0.07884076    111
## 25  1885-Cleveland      0   140      0 140    1816   9360 0.07709251     44
## 26   1889-Harrison      4   350      0 354    4721   9360 0.07498411    157
## 27  1893-Cleveland      0   125      0 125    2125   9360 0.05882353     58
## 28   1897-McKinley      6   399      0 405    4353   9360 0.09303928    130
## 29   1901-McKinley     10   232      1 242    2437   9360 0.09930242    100
## 30  1905-Roosevelt      0   101      0 101    1079   9360 0.09360519     33
## 31       1909-Taft      3   402      1 405    5821   9360 0.06957567    159
## 32     1913-Wilson      0   193      0 193    1882   9360 0.10255048     68
## 33     1917-Wilson      0   125      0 125    1652   9360 0.07566586     59
## 34    1921-Harding      0   411      0 411    3719   9360 0.11051358    148
## 35   1925-Coolidge      1   385      0 386    4440   9360 0.08693694    196
## 36     1929-Hoover      0   316      0 316    3860   9360 0.08186528    158
## 37  1933-Roosevelt      0   180      0 180    2057   9360 0.08750608     85
## 38  1937-Roosevelt      2   198      0 200    1989   9360 0.10055304     96
## 39  1941-Roosevelt      4   187      0 191    1519   9360 0.12574062     68
## 40  1945-Roosevelt      2    79      0  81     633   9360 0.12796209     26
## 41     1949-Truman      1   244      0 245    2504   9360 0.09784345    116
## 42 1953-Eisenhower      9   305      0 314    2743   9360 0.11447320    119
## 43 1957-Eisenhower      0   253      0 253    1907   9360 0.13266911     92
## 44    1961-Kennedy      2   179      0 180    1541   9360 0.11680727     52
## 45    1965-Johnson      3   226      0 229    1710   9360 0.13391813     93
## 46      1969-Nixon      1   299      0 300    2416   9360 0.12417219    103
## 47      1973-Nixon      3   210      0 213    1995   9360 0.10676692     68
## 48     1977-Carter      3   152      0 155    1369   9360 0.11322133     52
## 49     1981-Reagan      2   362      0 363    2780   9360 0.13057554    128
## 50     1985-Reagan     13   366      0 379    2909   9360 0.13028532    123
## 51       1989-Bush      2   377      0 379    2673   9360 0.14178825    141
## 52    1993-Clinton      1   247      0 248    1833   9360 0.13529733     81
## 53    1997-Clinton      9   293      0 302    2436   9360 0.12397373    111
## 54       2001-Bush      1   232      0 233    1806   9360 0.12901440     97
## 55       2005-Bush      0   254      0 254    2312   9360 0.10986159     99
## 56      2009-Obama      0   321      0 321    2689   9360 0.11937523    110
## 57      2013-Obama      5   242      0 247    2317   9360 0.10660337     88
## 58      2017-Trump      2   225      0 227    1660   9360 0.13674699     88
##    is_dup
## 1   FALSE
## 2   FALSE
## 3   FALSE
## 4   FALSE
## 5   FALSE
## 6   FALSE
## 7   FALSE
## 8   FALSE
## 9   FALSE
## 10  FALSE
## 11  FALSE
## 12  FALSE
## 13  FALSE
## 14  FALSE
## 15  FALSE
## 16  FALSE
## 17  FALSE
## 18  FALSE
## 19  FALSE
## 20  FALSE
## 21  FALSE
## 22  FALSE
## 23  FALSE
## 24  FALSE
## 25  FALSE
## 26  FALSE
## 27  FALSE
## 28  FALSE
## 29  FALSE
## 30  FALSE
## 31  FALSE
## 32  FALSE
## 33  FALSE
## 34  FALSE
## 35  FALSE
## 36  FALSE
## 37  FALSE
## 38  FALSE
## 39  FALSE
## 40  FALSE
## 41  FALSE
## 42  FALSE
## 43  FALSE
## 44  FALSE
## 45  FALSE
## 46  FALSE
## 47  FALSE
## 48  FALSE
## 49  FALSE
## 50  FALSE
## 51  FALSE
## 52  FALSE
## 53  FALSE
## 54  FALSE
## 55  FALSE
## 56  FALSE
## 57  FALSE
## 58  FALSE

data_corpus_inaugural %>%
  mutate(fullname = paste(FirstName, President, sep = ", ")) %>%
  summary(n = 5)
## Warning: n argument is not used.
##             doc_id number punct symbol any n_token n_type      noise n_sent
## 1  1789-Washington      1   108      0 109    1537   9360 0.07091737     23
## 2  1793-Washington      0    12      0  12     147   9360 0.08163265      4
## 3       1797-Adams      0   263      0 263    2577   9360 0.10205666     37
## 4   1801-Jefferson      0   203      0 203    1923   9360 0.10556422     41
## 5   1805-Jefferson      0   216      0 216    2380   9360 0.09075630     45
## 6     1809-Madison      0    88      0  88    1261   9360 0.06978588     21
## 7     1813-Madison      0    93      0  93    1302   9360 0.07142857     33
## 8      1817-Monroe      0   315      0 315    3677   9360 0.08566766    121
## 9      1821-Monroe     13   424      2 436    4886   9360 0.08923455    129
## 10      1825-Adams      0   234      0 234    3147   9360 0.07435653     74
## 11    1829-Jackson      0    80      0  80    1208   9360 0.06622517     25
## 12    1833-Jackson      0    92      0  92    1267   9360 0.07261247     29
## 13   1837-VanBuren      0   326      0 326    4158   9360 0.07840308     95
## 14   1841-Harrison      0   702      0 702    9123   9360 0.07694837    210
## 15       1845-Polk      1   394      0 395    5186   9360 0.07616660    153
## 16     1849-Taylor      0    93      0  93    1178   9360 0.07894737     22
## 17     1853-Pierce      1   314      0 315    3636   9360 0.08663366    104
## 18   1857-Buchanan      0   271      0 271    3083   9360 0.08790139     89
## 19    1861-Lincoln      4   370      0 374    3999   9360 0.09352338    135
## 20    1865-Lincoln      0    82      0  82     775   9360 0.10580645     26
## 21      1869-Grant      0   107      0 107    1229   9360 0.08706265     40
## 22      1873-Grant      2   137      0 139    1472   9360 0.09442935     43
## 23      1877-Hayes      2   231      0 232    2707   9360 0.08570373     59
## 24   1881-Garfield      4   250      0 253    3209   9360 0.07884076    111
## 25  1885-Cleveland      0   140      0 140    1816   9360 0.07709251     44
## 26   1889-Harrison      4   350      0 354    4721   9360 0.07498411    157
## 27  1893-Cleveland      0   125      0 125    2125   9360 0.05882353     58
## 28   1897-McKinley      6   399      0 405    4353   9360 0.09303928    130
## 29   1901-McKinley     10   232      1 242    2437   9360 0.09930242    100
## 30  1905-Roosevelt      0   101      0 101    1079   9360 0.09360519     33
## 31       1909-Taft      3   402      1 405    5821   9360 0.06957567    159
## 32     1913-Wilson      0   193      0 193    1882   9360 0.10255048     68
## 33     1917-Wilson      0   125      0 125    1652   9360 0.07566586     59
## 34    1921-Harding      0   411      0 411    3719   9360 0.11051358    148
## 35   1925-Coolidge      1   385      0 386    4440   9360 0.08693694    196
## 36     1929-Hoover      0   316      0 316    3860   9360 0.08186528    158
## 37  1933-Roosevelt      0   180      0 180    2057   9360 0.08750608     85
## 38  1937-Roosevelt      2   198      0 200    1989   9360 0.10055304     96
## 39  1941-Roosevelt      4   187      0 191    1519   9360 0.12574062     68
## 40  1945-Roosevelt      2    79      0  81     633   9360 0.12796209     26
## 41     1949-Truman      1   244      0 245    2504   9360 0.09784345    116
## 42 1953-Eisenhower      9   305      0 314    2743   9360 0.11447320    119
## 43 1957-Eisenhower      0   253      0 253    1907   9360 0.13266911     92
## 44    1961-Kennedy      2   179      0 180    1541   9360 0.11680727     52
## 45    1965-Johnson      3   226      0 229    1710   9360 0.13391813     93
## 46      1969-Nixon      1   299      0 300    2416   9360 0.12417219    103
## 47      1973-Nixon      3   210      0 213    1995   9360 0.10676692     68
## 48     1977-Carter      3   152      0 155    1369   9360 0.11322133     52
## 49     1981-Reagan      2   362      0 363    2780   9360 0.13057554    128
## 50     1985-Reagan     13   366      0 379    2909   9360 0.13028532    123
## 51       1989-Bush      2   377      0 379    2673   9360 0.14178825    141
## 52    1993-Clinton      1   247      0 248    1833   9360 0.13529733     81
## 53    1997-Clinton      9   293      0 302    2436   9360 0.12397373    111
## 54       2001-Bush      1   232      0 233    1806   9360 0.12901440     97
## 55       2005-Bush      0   254      0 254    2312   9360 0.10986159     99
## 56      2009-Obama      0   321      0 321    2689   9360 0.11937523    110
## 57      2013-Obama      5   242      0 247    2317   9360 0.10660337     88
## 58      2017-Trump      2   225      0 227    1660   9360 0.13674699     88
##    is_dup
## 1   FALSE
## 2   FALSE
## 3   FALSE
## 4   FALSE
## 5   FALSE
## 6   FALSE
## 7   FALSE
## 8   FALSE
## 9   FALSE
## 10  FALSE
## 11  FALSE
## 12  FALSE
## 13  FALSE
## 14  FALSE
## 15  FALSE
## 16  FALSE
## 17  FALSE
## 18  FALSE
## 19  FALSE
## 20  FALSE
## 21  FALSE
## 22  FALSE
## 23  FALSE
## 24  FALSE
## 25  FALSE
## 26  FALSE
## 27  FALSE
## 28  FALSE
## 29  FALSE
## 30  FALSE
## 31  FALSE
## 32  FALSE
## 33  FALSE
## 34  FALSE
## 35  FALSE
## 36  FALSE
## 37  FALSE
## 38  FALSE
## 39  FALSE
## 40  FALSE
## 41  FALSE
## 42  FALSE
## 43  FALSE
## 44  FALSE
## 45  FALSE
## 46  FALSE
## 47  FALSE
## 48  FALSE
## 49  FALSE
## 50  FALSE
## 51  FALSE
## 52  FALSE
## 53  FALSE
## 54  FALSE
## 55  FALSE
## 56  FALSE
## 57  FALSE
## 58  FALSE
```

Filtering documents based on years:

``` r
data_corpus_inaugural %>%
  filter(President == "Roosevelt") %>%
  summary()
##           doc_id number punct symbol any n_token n_type      noise n_sent
## 1 1905-Roosevelt      0   101      0 101    1079   1628 0.09360519     33
## 2 1933-Roosevelt      0   180      0 180    2057   1628 0.08750608     85
## 3 1937-Roosevelt      2   198      0 200    1989   1628 0.10055304     96
## 4 1941-Roosevelt      4   187      0 191    1519   1628 0.12574062     68
## 5 1945-Roosevelt      2    79      0  81     633   1628 0.12796209     26
##   is_dup
## 1  FALSE
## 2  FALSE
## 3  FALSE
## 4  FALSE
## 5  FALSE
```

Renaming document variables:

``` r
data_corpus_inaugural %>%
  rename(LastName = President) %>%
  select(FirstName, LastName) %>%
  summary(n = 5)
## Warning: n argument is not used.
##             doc_id number punct symbol any n_token n_type      noise n_sent
## 1  1789-Washington      1   108      0 109    1537   9360 0.07091737     23
## 2  1793-Washington      0    12      0  12     147   9360 0.08163265      4
## 3       1797-Adams      0   263      0 263    2577   9360 0.10205666     37
## 4   1801-Jefferson      0   203      0 203    1923   9360 0.10556422     41
## 5   1805-Jefferson      0   216      0 216    2380   9360 0.09075630     45
## 6     1809-Madison      0    88      0  88    1261   9360 0.06978588     21
## 7     1813-Madison      0    93      0  93    1302   9360 0.07142857     33
## 8      1817-Monroe      0   315      0 315    3677   9360 0.08566766    121
## 9      1821-Monroe     13   424      2 436    4886   9360 0.08923455    129
## 10      1825-Adams      0   234      0 234    3147   9360 0.07435653     74
## 11    1829-Jackson      0    80      0  80    1208   9360 0.06622517     25
## 12    1833-Jackson      0    92      0  92    1267   9360 0.07261247     29
## 13   1837-VanBuren      0   326      0 326    4158   9360 0.07840308     95
## 14   1841-Harrison      0   702      0 702    9123   9360 0.07694837    210
## 15       1845-Polk      1   394      0 395    5186   9360 0.07616660    153
## 16     1849-Taylor      0    93      0  93    1178   9360 0.07894737     22
## 17     1853-Pierce      1   314      0 315    3636   9360 0.08663366    104
## 18   1857-Buchanan      0   271      0 271    3083   9360 0.08790139     89
## 19    1861-Lincoln      4   370      0 374    3999   9360 0.09352338    135
## 20    1865-Lincoln      0    82      0  82     775   9360 0.10580645     26
## 21      1869-Grant      0   107      0 107    1229   9360 0.08706265     40
## 22      1873-Grant      2   137      0 139    1472   9360 0.09442935     43
## 23      1877-Hayes      2   231      0 232    2707   9360 0.08570373     59
## 24   1881-Garfield      4   250      0 253    3209   9360 0.07884076    111
## 25  1885-Cleveland      0   140      0 140    1816   9360 0.07709251     44
## 26   1889-Harrison      4   350      0 354    4721   9360 0.07498411    157
## 27  1893-Cleveland      0   125      0 125    2125   9360 0.05882353     58
## 28   1897-McKinley      6   399      0 405    4353   9360 0.09303928    130
## 29   1901-McKinley     10   232      1 242    2437   9360 0.09930242    100
## 30  1905-Roosevelt      0   101      0 101    1079   9360 0.09360519     33
## 31       1909-Taft      3   402      1 405    5821   9360 0.06957567    159
## 32     1913-Wilson      0   193      0 193    1882   9360 0.10255048     68
## 33     1917-Wilson      0   125      0 125    1652   9360 0.07566586     59
## 34    1921-Harding      0   411      0 411    3719   9360 0.11051358    148
## 35   1925-Coolidge      1   385      0 386    4440   9360 0.08693694    196
## 36     1929-Hoover      0   316      0 316    3860   9360 0.08186528    158
## 37  1933-Roosevelt      0   180      0 180    2057   9360 0.08750608     85
## 38  1937-Roosevelt      2   198      0 200    1989   9360 0.10055304     96
## 39  1941-Roosevelt      4   187      0 191    1519   9360 0.12574062     68
## 40  1945-Roosevelt      2    79      0  81     633   9360 0.12796209     26
## 41     1949-Truman      1   244      0 245    2504   9360 0.09784345    116
## 42 1953-Eisenhower      9   305      0 314    2743   9360 0.11447320    119
## 43 1957-Eisenhower      0   253      0 253    1907   9360 0.13266911     92
## 44    1961-Kennedy      2   179      0 180    1541   9360 0.11680727     52
## 45    1965-Johnson      3   226      0 229    1710   9360 0.13391813     93
## 46      1969-Nixon      1   299      0 300    2416   9360 0.12417219    103
## 47      1973-Nixon      3   210      0 213    1995   9360 0.10676692     68
## 48     1977-Carter      3   152      0 155    1369   9360 0.11322133     52
## 49     1981-Reagan      2   362      0 363    2780   9360 0.13057554    128
## 50     1985-Reagan     13   366      0 379    2909   9360 0.13028532    123
## 51       1989-Bush      2   377      0 379    2673   9360 0.14178825    141
## 52    1993-Clinton      1   247      0 248    1833   9360 0.13529733     81
## 53    1997-Clinton      9   293      0 302    2436   9360 0.12397373    111
## 54       2001-Bush      1   232      0 233    1806   9360 0.12901440     97
## 55       2005-Bush      0   254      0 254    2312   9360 0.10986159     99
## 56      2009-Obama      0   321      0 321    2689   9360 0.11937523    110
## 57      2013-Obama      5   242      0 247    2317   9360 0.10660337     88
## 58      2017-Trump      2   225      0 227    1660   9360 0.13674699     88
##    is_dup
## 1   FALSE
## 2   FALSE
## 3   FALSE
## 4   FALSE
## 5   FALSE
## 6   FALSE
## 7   FALSE
## 8   FALSE
## 9   FALSE
## 10  FALSE
## 11  FALSE
## 12  FALSE
## 13  FALSE
## 14  FALSE
## 15  FALSE
## 16  FALSE
## 17  FALSE
## 18  FALSE
## 19  FALSE
## 20  FALSE
## 21  FALSE
## 22  FALSE
## 23  FALSE
## 24  FALSE
## 25  FALSE
## 26  FALSE
## 27  FALSE
## 28  FALSE
## 29  FALSE
## 30  FALSE
## 31  FALSE
## 32  FALSE
## 33  FALSE
## 34  FALSE
## 35  FALSE
## 36  FALSE
## 37  FALSE
## 38  FALSE
## 39  FALSE
## 40  FALSE
## 41  FALSE
## 42  FALSE
## 43  FALSE
## 44  FALSE
## 45  FALSE
## 46  FALSE
## 47  FALSE
## 48  FALSE
## 49  FALSE
## 50  FALSE
## 51  FALSE
## 52  FALSE
## 53  FALSE
## 54  FALSE
## 55  FALSE
## 56  FALSE
## 57  FALSE
## 58  FALSE
```

Glimpse (from **tibble**):

``` r
glimpse(data_corpus_inaugural)
## Rows: 58
## Columns: 6
## $ doc_id    <chr> "1789-Washington", "1793-Washington", "1797-Adams", "1801-J…
## $ text      <chr> "Fellow-Cit…", "Fellow cit…", "When it wa…", "Friends an…",…
## $ Year      <int> 1789, 1793, 1797, 1801, 1805, 1809, 1813, 1817, 1821, 1825,…
## $ President <chr> "Washington", "Washington", "Adams", "Jefferson", "Jefferso…
## $ FirstName <chr> "George", "George", "John", "Thomas", "Thomas", "James", "J…
## $ Party     <fct> none, none, Federalist, Democratic-Republican, Democratic-R…
```
