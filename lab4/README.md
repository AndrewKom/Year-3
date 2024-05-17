# Практическая работа 4

## Цель работы

1.  Изучить возможности СУБД DuckDB для обработки и анализ больших
    данных
2.  Получить навыки применения DuckDB совместно с языком
    программирования R
3.  Получить навыки анализа метаинфомации о сетевом трафике
4.  Получить навыки применения облачных технологий хранения, подготовки
    и анализа данных: Yandex Object Storage, Rstudio Server

## Исходные данные

1.  ОС Windows
2.  DuckDB
3.  Apache Arrow
4.  RStudio

## План

1.  Выполним практическое задание
2.  Составим отчет

## Описание шагов:

1.  Выполним практические задания

``` r
library(arrow, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::duration() masks arrow::duration()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ dplyr::lag()          masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(duckdb, warn.conflicts = FALSE)
```

    Загрузка требуемого пакета: DBI

``` r
connection <- dbConnect(duckdb::duckdb(), dbdir = "")
dbExecute(conn = connection, "INSTALL httpfs; LOAD httpfs;")
```

    [1] 0

``` r
PQF = "https://storage.yandexcloud.net/arrow-datasets/tm_data.pqt"
SQL <- "SELECT * FROM read_parquet([?])"
data <- dbGetQuery(connection, SQL, list(PQF))
```

### Задание 1: Найдите утечку данных из Вашей сети

Важнейшие документы с результатами нашей исследовательской деятельности
в области создания вакцин скачиваются в виде больших заархивированных
дампов. Один из хостов в нашей сети используется для пересылки этой
информации – он пересылает гораздо больше информации на внешние ресурсы
в Интернете, чем остальные компьютеры нашей сети. Определите его
IP-адрес

``` r
z1 <- data %>%
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(!grepl('^1[2-4].*', dst))  %>%
  group_by(src) %>% summarise("sum" = sum(bytes)) %>%  
  arrange(desc(sum)) %>% select(src,sum) %>% slice(1)
z1 |> collect()
```

    # A tibble: 1 × 2
      src                  sum
      <chr>              <dbl>
    1 13.37.84.125 10625497574

Ответ  
13.37.84.125

### Задание 2: Найдите утечку данных 2

Другой атакующий установил автоматическую задачу в системном
планировщике cron для экспорта содержимого внутренней wiki системы. Эта
система генерирует большое количество трафика в нерабочие часы, больше
чем остальные хосты. Определите IP этой системы. Известно, что ее IP
адрес отличается от нарушителя из предыдущей задачи

``` r
z2 <- data %>%
      select(timestamp, src, dst, bytes) %>%
      mutate(trafic = (str_detect(src, "^((12|13|14)\\.)") & !str_detect(dst, "^((12|13|14)\\.)")),time = hour(as_datetime(timestamp/1000))) %>%
      filter(trafic == TRUE, time >= 0 & time <= 24) %>% group_by(time) %>%
      summarise(trafictime = n()) %>% arrange(desc(trafictime))
z2 |> collect()
```

    # A tibble: 24 × 2
        time trafictime
       <int>      <int>
     1    16    4490576
     2    22    4489703
     3    18    4489386
     4    23    4488093
     5    19    4487345
     6    21    4487109
     7    17    4483578
     8    20    4482712
     9    13     169617
    10     7     169241
    # ℹ 14 more rows

Определяем рабочие часы:  
Учитывая нагрузку на трафик, рабочее время: 16:00-23:00

``` r
z2_2 <- data %>% mutate(time = hour(as_datetime(timestamp/1000))) %>% 
  filter(!str_detect(src, "^13.37.84.125")) %>% 
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(!grepl('^1[2-4].*', dst))  %>%
  filter(time >= 0 & time <= 15) %>% 
  group_by(src) %>% summarise("sum" = sum(bytes)) %>%
  arrange(desc(sum)) %>% select(src,sum) %>% slice(1)
z2_2 |> collect()
```

    # A tibble: 1 × 2
      src               sum
      <chr>           <int>
    1 12.55.77.96 289566918

Ответ  
12.55.77.96

### Задание 3: Найдите утечку данных 3

Еще один нарушитель собирает содержимое электронной почты и отправляет в
Интернет используя порт, который обычно используется для другого типа
трафика. Атакующий пересылает большое количество информации используя
этот порт,которое нехарактерно для других хостов, использующих этот
номер порта. Определите IP этой системы. Известно, что ее IP адрес
отличается от нарушителей из предыдущих задач

``` r
z3 <- data %>% filter(!str_detect(src, "^13.37.84.125")) %>% 
  filter(!str_detect(src, "^12.55.77.96")) %>% 
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(!str_detect(dst, "^12.") | !str_detect(dst, "^13.") | !str_detect(dst, "^14."))  %>% select(src, bytes, port) 


z3_1 <-z3 %>%  group_by(port) %>% summarise("mean"=mean(bytes), "max"=max(bytes), "sum" = sum(bytes)) %>% 
  mutate("Raz"= max-mean)  %>% filter(Raz!=0, Raz>170000)

z3_1 |> collect()
```

    # A tibble: 1 × 5
       port   mean    max         sum     Raz
      <int>  <dbl>  <int>       <dbl>   <dbl>
    1    37 33348. 209402 48192673159 176054.

``` r
z3_2 <- z3  %>% filter(port==37) %>% group_by(src) %>% 
  summarise("mean"=mean(bytes)) %>% filter(mean>37543) %>% select(src)
z3_2 |> collect()
```

    # A tibble: 1 × 1
      src        
      <chr>      
    1 13.46.35.35

Ответ  
13.46.35.35

### Задание 4: Обнаружение канала управления

Зачастую в корпоротивных сетях находятся ранее зараженные системы,
компрометация которых осталась незамеченной. Такие системы генерируют
небольшое количество трафика для связи с панелью управления бот-сети, но
с одинаковыми параметрами – в данном случае с одинаковым номером порта.

Како номер порта использует бот-панель для управления ботами?

``` r
z4 <- data %>% group_by(port) %>% 
  summarise(z4_1 = mean(bytes)-min(bytes)) %>% arrange(z4_1) %>% filter(z4_1!=0) %>% select(port,z4_1) %>% slice(1)
z4 |> collect()
```

    # A tibble: 1 × 2
       port  z4_1
      <int> <dbl>
    1   124 0.327

Ответ  
124

### Задание 5: Обнаружение P2P трафика

Иногда компрометация сети проявляется в нехарактерном трафике между
хостами в локальной сети, который свидетельствует о горизонтальном
перемещении (lateral movement).

В нашей сети замечена система, которая ретранслирует по локальной сети
полученные от панели управления бот-сети команды, создав таким образом
внутреннюю пиринговую сеть.

Какой уникальный порт используется этой бот сетью для внутреннего
общения между собой?

``` r
z5 <- data %>%
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(str_detect(dst, "^12.") | str_detect(dst, "^13.") | str_detect(dst, "^14."))  %>% 
  group_by(port) %>%
  summarise(z5_1 = max(bytes) - min(bytes)) %>%
  arrange(desc(z5_1)) %>% select(port) %>% slice(1)
z5 |> collect()
```

    # A tibble: 1 × 1
       port
      <int>
    1   115

Ответ  
115

### Задание 6: Чемпион малвари

Нашу сеть только что внесли в списки спам-ферм. Один из хостов сети
получает множество команд от панели C&C, ретранслируя их внутри сети. В
обычных условиях причин для такого активного взаимодействия внутри сети
у данного хоста нет.

Определите IP такого хоста.

``` r
z6 <- data %>%
  filter(str_detect(src, "^12.") | str_detect(src, "^13.") | str_detect(src, "^14."))  %>%
  filter(str_detect(dst, "^12.") | str_detect(dst, "^13.") | str_detect(dst, "^14."))  %>% group_by(src) %>% summarise(count = n()) %>% arrange(desc(count)) %>% slice(1)
z6 |> collect()
```

    # A tibble: 1 × 2
      src         count
      <chr>       <int>
    1 13.42.70.40 65109

Ответ  
13.42.70.40

## Оценка результатов

Задача выполнена при помощи приложения RStudio, Apache Arrow, DuckDB,
удалось познакомится с функционалом Arrow и изучить возможности СУБД
DuckDB для анализа больших данных, при их помощи удалось выполнить
задание.

## Вывод

В данной работе я смог познакомиться с применением DuckDB совместно с
языком программирования R. Удалось проанализировать метаинформацию о
сетевом трафике.
