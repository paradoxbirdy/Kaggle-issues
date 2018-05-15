---
title: "For Next Donation"
author: "Shi Keqing"
output:
  html_document:
    code_folding: hide
    df_print: kable
    fig_caption: yes
    highlight: tango
    keep_md: yes
    number_sections: yes
    theme: cosmo
    toc: yes
    toc_depth: 4
  pdf_document:
    toc: yes
    toc_depth: '4'
---



# Intro & Purpose
## Background
[DonorsChoose.org](https://www.donorschoose.org/) has funded over 1.1 million classroom requests through the support of 3 million donors, the majority of whom were making their *first-ever donation* to a public school. If DonorsChoose.org can motivate even a fraction of those donors to make *another donation*, that could have a huge impact on the number of classroom requests fulfilled.

## Mission
A good solution will enable DonorsChoose.org to build **targeted email campaigns recommending specific classroom requests to prior donors**. Part of the challenge is to assess the needs of the organization, uncover insights from the data available, and build the right solution for this problem.

\pagebreak

# Load Packages and Data Importing
## Packages to Use
For this mission, we need to library packages for data-manipulation, visualization, RMarkdown writing and  non-supervised algorithm.


```r
# Reproducible-Research 
library('knitr')

# Data-Manipulation
library('data.table')
library('tibble')

# Visualization
library('ggplot2')
```

## Data Importing
All the data provided by csv format, in **6 files** with size of **3.71 GB** totally on Windows.


```r
donations <- as.tibble(fread(input = "D:/DataSet/Kaggle/DonorsChoose/Donations.csv", header = TRUE, sep = ","))

donors <- as.tibble(fread("D:/DataSet/Kaggle/DonorsChoose/Donors.csv", header = TRUE, sep = ","))

projects <- as.tibble(fread("D:/DataSet/Kaggle/DonorsChoose/Projects.csv", header = TRUE, sep = ","))

resources <- as.tibble(fread("D:/DataSet/Kaggle/DonorsChoose/Resources.csv", header = TRUE, sep = ","))
                             
schools <- as.tibble(fread("D:/DataSet/Kaggle/DonorsChoose/Schools.csv", header = TRUE, sep = ","))

teachers <- as.tibble(fread("D:/DataSet/Kaggle/DonorsChoose/Teachers.csv", header = TRUE, sep = ","))
```

\pagebreak

# First Look of the Data Files{.tabset .tabset-fade .tabset-pills} 
## Project Data{.tabset}
This dataset contains information on all projects posted on [DonorsChoose.org](https://www.donorschoose.org/) site in the past five years and four months, between **2013-01-01** and **2018-05-01**. Each row in this dataset represents a single project and each column is an attribute of the project.

### structure 

```r
glimpse(projects)
```

```
## Observations: 1,208,651
## Variables: 15
## $ `Project ID`                       <chr> "77b7d3f2ac4e32d538914e4a8c...
## $ `School ID`                        <chr> "c2d5cb0a29a62e72cdccee939f...
## $ `Teacher ID`                       <chr> "59f7d2c62f7e76a99d31db6f62...
## $ `Teacher Project Posted Sequence`  <chr> "2", "1", "2", "1", "1", "2...
## $ `Project Type`                     <chr> "Teacher-Led", "Teacher-Led...
## $ `Project Title`                    <chr> "Anti-Bullying Begins with ...
## $ `Project Essay`                    <chr> "do you remember your favor...
## $ `Project Subject Category Tree`    <chr> "Applied Learning, Literacy...
## $ `Project Subject Subcategory Tree` <chr> "Character Education, Liter...
## $ `Project Grade Level Category`     <chr> "Grades PreK-2", "Grades 6-...
## $ `Project Resource Category`        <chr> "Books", "Supplies", "Books...
## $ `Project Cost`                     <chr> "$490.38", "$420.61", "$510...
## $ `Project Posted Date`              <chr> "2013-01-01", "2013-01-01",...
## $ `Project Current Status`           <chr> "Fully Funded", "Expired", ...
## $ `Project Fully Funded Date`        <chr> "2013-03-12", "", "2013-01-...
```

### summary

```r
summary(projects)
```

```
##   Project ID         School ID          Teacher ID       
##  Length:1208651     Length:1208651     Length:1208651    
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##  Teacher Project Posted Sequence Project Type       Project Title     
##  Length:1208651                  Length:1208651     Length:1208651    
##  Class :character                Class :character   Class :character  
##  Mode  :character                Mode  :character   Mode  :character  
##  Project Essay      Project Subject Category Tree
##  Length:1208651     Length:1208651               
##  Class :character   Class :character             
##  Mode  :character   Mode  :character             
##  Project Subject Subcategory Tree Project Grade Level Category
##  Length:1208651                   Length:1208651              
##  Class :character                 Class :character            
##  Mode  :character                 Mode  :character            
##  Project Resource Category Project Cost       Project Posted Date
##  Length:1208651            Length:1208651     Length:1208651     
##  Class :character          Class :character   Class :character   
##  Mode  :character          Mode  :character   Mode  :character   
##  Project Current Status Project Fully Funded Date
##  Length:1208651         Length:1208651           
##  Class :character       Class :character         
##  Mode  :character       Mode  :character
```

### head
Belowing table neglects column **Project Essay** because of the column includes essay text for describing, not suitable and necessary for show here. 

```r
knitr::kable(head(projects[1:4]), format = "markdown")
```



|Project ID                       |School ID                        |Teacher ID                       |Teacher Project Posted Sequence |
|:--------------------------------|:--------------------------------|:--------------------------------|:-------------------------------|
|77b7d3f2ac4e32d538914e4a8cb8a525 |c2d5cb0a29a62e72cdccee939f434181 |59f7d2c62f7e76a99d31db6f62b7b67c |2                               |
|fd928b7f6386366a9cad2bea40df4b25 |8acbb544c9215b25c71a0c655200baea |8fbd92394e20d647ddcdc6085ce1604b |1                               |
|7c915e8e1d27f10a94abd689e99c336f |0ae85ea7c7acc41cffa9f81dc61d46df |9140ac16d2e6cee45bd50b0b2ce8cd04 |2                               |
|feeec44c2a3a3d9a31c137a9780d2521 |deddcdb20f86599cefa5e7eb31da309b |63750e765b46f9fa4d71e780047e896e |1                               |
|037719bf60853f234610458a210f45a9 |f3f0dc60ba3026944eeffe8b76cb8d9c |0d5b4cc12b2eb00013460d0ac38ce2a2 |1                               |
|c47df2c15d5add778d7c5beae0771c7a |4e41e5f15c31a05d3d3260126bacba1a |11356cf12a4a4ef658294cb7eda6e13b |2                               |

```r
knitr::kable(head(projects[c(5:6,8:9)]), format = "markdown")
```



|Project Type |Project Title                                    |Project Subject Category Tree         |Project Subject Subcategory Tree          |
|:------------|:------------------------------------------------|:-------------------------------------|:-----------------------------------------|
|Teacher-Led  |Anti-Bullying Begins with Me                     |Applied Learning, Literacy & Language |Character Education, Literacy             |
|Teacher-Led  |Ukuleles For Middle Schoolers                    |Music & The Arts                      |Music                                     |
|Teacher-Led  |Big Books, Flip Books, And Everything In Between |Literacy & Language, Special Needs    |Literacy, Special Needs                   |
|Teacher-Led  |A Little for a Lot                               |Math & Science, Literacy & Language   |Applied Sciences, Literature & Writing    |
|Teacher-Led  |Technology in the Classroom                      |Literacy & Language, Math & Science   |Literacy, Mathematics                     |
|Teacher-Led  |Growing Up Behind the Iron Curtain               |History & Civics, Literacy & Language |History & Geography, Literature & Writing |

```r
knitr::kable(head(projects[10:15]), format = "markdown")
```



|Project Grade Level Category |Project Resource Category |Project Cost |Project Posted Date |Project Current Status |Project Fully Funded Date |
|:----------------------------|:-------------------------|:------------|:-------------------|:----------------------|:-------------------------|
|Grades PreK-2                |Books                     |$490.38      |2013-01-01          |Fully Funded           |2013-03-12                |
|Grades 6-8                   |Supplies                  |$420.61      |2013-01-01          |Expired                |                          |
|Grades PreK-2                |Books                     |$510.46      |2013-01-01          |Fully Funded           |2013-01-07                |
|Grades 3-5                   |Supplies                  |$282.80      |2013-01-01          |Fully Funded           |2013-05-29                |
|Grades PreK-2                |Technology                |$555.28      |2013-01-01          |Fully Funded           |2013-02-14                |
|Grades 3-5                   |Books                     |$565.56      |2013-01-01          |Expired                |                          |

## Schools Data{.tabset}
This dataset contains information at a school level. Each row represents a single school. This dataset is joined with the project data using the **School ID** column.

### structure 

```r
glimpse(schools)
```

```
## Observations: 72,993
## Variables: 9
## $ `School ID`                    <chr> "00003e0fdd601b8ea0a6eb44057b9c...
## $ `School Name`                  <chr> "Capon Bridge Middle School", "...
## $ `School Metro Type`            <chr> "rural", "urban", "suburban", "...
## $ `School Percentage Free Lunch` <int> 56, 41, 2, 76, 50, 63, 17, 15, ...
## $ `School State`                 <chr> "West Virginia", "Texas", "Wash...
## $ `School Zip`                   <int> 26711, 77384, 98074, 48370, 755...
## $ `School City`                  <chr> "Capon Bridge", "The Woodlands"...
## $ `School County`                <chr> "Hampshire", "Montgomery", "Kin...
## $ `School District`              <chr> "Hampshire Co School District",...
```

### summary

```r
summary(schools)
```

```
##   School ID         School Name        School Metro Type 
##  Length:72993       Length:72993       Length:72993      
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  School Percentage Free Lunch School State         School Zip   
##  Min.   :  0.00               Length:72993       Min.   :  705  
##  1st Qu.: 40.00               Class :character   1st Qu.:29554  
##  Median : 61.00               Mode  :character   Median :53095  
##  Mean   : 58.56                                  Mean   :53382  
##  3rd Qu.: 80.00                                  3rd Qu.:78572  
##  Max.   :100.00                                  Max.   :99950  
##  NA's   :1141                                                   
##  School City        School County      School District   
##  Length:72993       Length:72993       Length:72993      
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
## 
```

### head

```r
knitr::kable(head(schools[1:4]), format = "markdown")
```



|School ID                        |School Name                            |School Metro Type | School Percentage Free Lunch|
|:--------------------------------|:--------------------------------------|:-----------------|----------------------------:|
|00003e0fdd601b8ea0a6eb44057b9c5e |Capon Bridge Middle School             |rural             |                           56|
|00004e32a448b4832e1b993500bf0731 |The Woodlands College Park High School |urban             |                           41|
|0002021bb799f28de224f1acc1ff08c4 |Samantha Smith Elementary School       |suburban          |                            2|
|0004604f675212a8cac1161338265196 |Kingsbury Country Day School           |unknown           |                           76|
|0004c9d50bcf0cea990f844e58b5e2c3 |Redwater Elementary School             |rural             |                           50|
|0004ffe3558fd70d939ad522b92447c8 |Math & Science Success Academy         |unknown           |                           63|

```r
knitr::kable(head(schools[5:9]), format = "markdown")
```



|School State  | School Zip|School City   |School County |School District              |
|:-------------|----------:|:-------------|:-------------|:----------------------------|
|West Virginia |      26711|Capon Bridge  |Hampshire     |Hampshire Co School District |
|Texas         |      77384|The Woodlands |Montgomery    |Conroe Ind School District   |
|Washington    |      98074|Sammamish     |King          |Lake Washington Sch Dist 414 |
|Michigan      |      48370|Oxford        |Oakland       |Michigan Dept Of Education   |
|Texas         |      75573|Redwater      |Bowie         |Redwater Ind Sch District    |
|Arizona       |      85706|Tucson        |Pima          |Arizona Dept Of Education    |

## Teachers Data{.tabset}
This dataset contains information at a teacher level. Each row represents a single teacher. This dataset is joined with the project data using the **Teacher ID** column.

### structure 

```r
glimpse(teachers)
```

```
## Observations: 402,900
## Variables: 3
## $ `Teacher ID`                        <chr> "00000f7264c27ba6fea0c837e...
## $ `Teacher Prefix`                    <chr> "Mrs.", "Mrs.", "Mr.", "Ms...
## $ `Teacher First Project Posted Date` <chr> "2013-08-21", "2016-10-23"...
```

### summary

```r
summary(teachers)
```

```
##   Teacher ID        Teacher Prefix     Teacher First Project Posted Date
##  Length:402900      Length:402900      Length:402900                    
##  Class :character   Class :character   Class :character                 
##  Mode  :character   Mode  :character   Mode  :character
```

### head

```r
knitr::kable(head(teachers), format = "markdown")
```



|Teacher ID                       |Teacher Prefix |Teacher First Project Posted Date |
|:--------------------------------|:--------------|:---------------------------------|
|00000f7264c27ba6fea0c837ed6aa0aa |Mrs.           |2013-08-21                        |
|00002d44003ed46b066607c5455a999a |Mrs.           |2016-10-23                        |
|00006084c3d92d904a22e0a70f5c119a |Mr.            |2016-09-08                        |
|0000a9af8b6b9cc9e41f53322a8b8cf1 |Ms.            |2015-10-25                        |
|0000d4777d14b33a1406dd6c9019fe89 |Ms.            |2017-02-10                        |
|0000fc11407901bcacdfad1db909b9f6 |Mrs.           |2015-06-22                        |

## Resources Data{.tabset}
For every project in the above dataset, there are one or more resources that is requested. This dataset contains the names of each resource in the project request and is joined with the dataset above using the **Project ID** column.

### structure 

```r
glimpse(resources)
```

```
## Observations: 7,210,448
## Variables: 5
## $ `Project ID`           <chr> "000009891526c0ade7180f8423792063", "00...
## $ `Resource Item Name`   <chr> "chair move and store cart", "sony mdr ...
## $ `Resource Quantity`    <dbl> 1, 40, 4, 1, 1, 2, 3, 1, 1, 1, 1, 1, 1,...
## $ `Resource Unit Price`  <dbl> 350.00, 12.86, 19.00, 269.00, 131.85, 3...
## $ `Resource Vendor Name` <chr> "", "CDW-G", "Amazon Business", "Lakesh...
```

### summary

```r
summary(resources)
```

```
##   Project ID        Resource Item Name Resource Quantity 
##  Length:7210448     Length:7210448     Min.   :   0.000  
##  Class :character   Class :character   1st Qu.:   1.000  
##  Mode  :character   Mode  :character   Median :   1.000  
##                                        Mean   :   2.817  
##                                        3rd Qu.:   2.000  
##                                        Max.   :4125.000  
##                                        NA's   :24299     
##  Resource Unit Price Resource Vendor Name
##  Min.   :    0.00    Length:7210448      
##  1st Qu.:    7.26    Class :character    
##  Median :   14.39    Mode  :character    
##  Mean   :   53.41                        
##  3rd Qu.:   36.40                        
##  Max.   :97085.50                        
##  NA's   :24310
```

### head

```r
knitr::kable(head(resources), format = "markdown")
```



|Project ID                       |Resource Item Name                        | Resource Quantity| Resource Unit Price|Resource Vendor Name         |
|:--------------------------------|:-----------------------------------------|-----------------:|-------------------:|:----------------------------|
|000009891526c0ade7180f8423792063 |chair move and store cart                 |                 1|              350.00|                             |
|00000ce845c00cbf0686c992fc369df4 |sony mdr zx100 blk   headphones           |                40|               12.86|CDW-G                        |
|00002d44003ed46b066607c5455a999a |gaiam kids stay-n-play balance ball, grey |                 4|               19.00|Amazon Business              |
|00002d44003ed46b066607c5455a999a |cf520x - giant comfy pillows - set of 4   |                 1|              269.00|Lakeshore Learning Materials |
|00002d44003ed46b066607c5455a999a |serta lounger, mini, sky blue             |                 1|              131.85|Amazon Business              |
|00002d44003ed46b066607c5455a999a |big joe roma bean bag chair, spicy lime   |                 2|               33.88|Amazon Business              |


## Donations Data{.tabset}
For every project in the above dataset, there are one or more donations. This dataset contains each donation from a citizen donor and is joined with the dataset above using the **Project ID** column.

### structure 

```r
glimpse(donations)
```

```
## Observations: 4,687,884
## Variables: 7
## $ `Project ID`                          <chr> "000009891526c0ade7180f8...
## $ `Donation ID`                         <chr> "68872912085866622120852...
## $ `Donor ID`                            <chr> "1f4b5b6e68445c6c4a0509b...
## $ `Donation Included Optional Donation` <chr> "No", "Yes", "Yes", "Yes...
## $ `Donation Amount`                     <dbl> 178.37, 25.00, 20.00, 25...
## $ `Donor Cart Sequence`                 <int> 11, 2, 3, 1, 2, 1, 1, 2,...
## $ `Donation Received Date`              <chr> "2016-08-23 13:15:57", "...
```

### summary

```r
summary(donations)
```

```
##   Project ID        Donation ID          Donor ID        
##  Length:4687884     Length:4687884     Length:4687884    
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##  Donation Included Optional Donation Donation Amount   
##  Length:4687884                      Min.   :    0.01  
##  Class :character                    1st Qu.:   14.82  
##  Mode  :character                    Median :   25.00  
##                                      Mean   :   60.67  
##                                      3rd Qu.:   50.00  
##                                      Max.   :60000.00  
##  Donor Cart Sequence Donation Received Date
##  Min.   :    1.0     Length:4687884        
##  1st Qu.:    1.0     Class :character      
##  Median :    2.0     Mode  :character      
##  Mean   :  143.1                           
##  3rd Qu.:   12.0                           
##  Max.   :18116.0
```

### head

```r
knitr::kable(head(donations[1:3]), format = "markdown")
```



|Project ID                       |Donation ID                      |Donor ID                         |
|:--------------------------------|:--------------------------------|:--------------------------------|
|000009891526c0ade7180f8423792063 |688729120858666221208529ee3fc18e |1f4b5b6e68445c6c4a0509b3aca93f38 |
|000009891526c0ade7180f8423792063 |dcf1071da3aa3561f91ac689d1f73dee |4aaab6d244bf3599682239ed5591af8a |
|000009891526c0ade7180f8423792063 |18a234b9d1e538c431761d521ea7799d |0b0765dc9c759adc48a07688ba25e94e |
|000009891526c0ade7180f8423792063 |38d2744bf9138b0b57ed581c76c0e2da |377944ad61f72d800b25ec1862aec363 |
|000009891526c0ade7180f8423792063 |5a032791e31167a70206bfb86fb60035 |6d5b22d39e68c656071a842732c63a0c |
|000009891526c0ade7180f8423792063 |8cea27f0cc03f41f66aab96b284ae6a1 |896c75c9b8d9a91c759746e566cd3f37 |

```r
knitr::kable(head(donations[4:7]), format = "markdown")
```



|Donation Included Optional Donation | Donation Amount| Donor Cart Sequence|Donation Received Date |
|:-----------------------------------|---------------:|-------------------:|:----------------------|
|No                                  |          178.37|                  11|2016-08-23 13:15:57    |
|Yes                                 |           25.00|                   2|2016-06-06 20:05:23    |
|Yes                                 |           20.00|                   3|2016-06-06 14:08:46    |
|Yes                                 |           25.00|                   1|2016-05-15 10:23:04    |
|Yes                                 |           25.00|                   2|2016-05-17 01:23:38    |
|Yes                                 |           15.00|                   1|2016-06-04 17:58:55    |

## Donors Data{.tabset}
This dataset contains information at a donor level. Each row represents a single donor. This dataset is joined with the donation data using the **Donor ID** column.

### structure 

```r
glimpse(donors)
```

```
## Observations: 2,122,640
## Variables: 5
## $ `Donor ID`         <chr> "00000ce845c00cbf0686c992fc369df4", "000027...
## $ `Donor City`       <chr> "Evanston", "Appomattox", "Winton", "Indian...
## $ `Donor State`      <chr> "Illinois", "other", "California", "Indiana...
## $ `Donor Is Teacher` <chr> "No", "No", "Yes", "No", "No", "No", "No", ...
## $ `Donor Zip`        <chr> "602", "245", "953", "462", "075", "", "069...
```

### summary

```r
summary(donors)
```

```
##    Donor ID          Donor City        Donor State       
##  Length:2122640     Length:2122640     Length:2122640    
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##  Donor Is Teacher    Donor Zip        
##  Length:2122640     Length:2122640    
##  Class :character   Class :character  
##  Mode  :character   Mode  :character
```

### head

```r
knitr::kable(head(donors), format = "markdown")
```



|Donor ID                         |Donor City   |Donor State |Donor Is Teacher |Donor Zip |
|:--------------------------------|:------------|:-----------|:----------------|:---------|
|00000ce845c00cbf0686c992fc369df4 |Evanston     |Illinois    |No               |602       |
|00002783bc5d108510f3f9666c8b1edd |Appomattox   |other       |No               |245       |
|00002d44003ed46b066607c5455a999a |Winton       |California  |Yes              |953       |
|00002eb25d60a09c318efbd0797bffb5 |Indianapolis |Indiana     |No               |462       |
|0000300773fe015f870914b42528541b |Paterson     |New Jersey  |No               |075       |
|00004c31ce07c22148ee37acd0f814b9 |             |other       |No               |          |

\pagebreak

# Missing Value

# Explore Data Analysis
Based on the files and tables above, we can see that there are a lot columns and infomations.So I do not think it's a good idea that we follow the traditional way to explore the data **one column by one column** for more infomations, not to mention we are really lack of domain knowledges and understanding of funding. In my opinion, focusing on some basic facts may discover more useful experiences, such as the following:
    + to be continued...

## Teacher

### When Teacher 

```r
teachers$Year_Month <- substr(teachers$`Teacher First Project Posted Date`, start = 1, stop = 7)
teachers_by_month_with_prefix <- as.data.frame(table(teachers$`Teacher Prefix`,
                                                     teachers$Year_Month))
names(teachers_by_month_with_prefix) <- c("Prefix","Year_Month", "Amount")
teachers_by_month_with_prefix <- teachers_by_month_with_prefix[which(teachers_by_month_with_prefix$Prefix != ""),]
ggplot(teachers_by_month_with_prefix, aes(x = `Year_Month`, y = `Amount`, 
                                          colour = `Prefix`, group = `Prefix`)) + 
    geom_line(size = 1.2) + geom_point(size = 1.5) + scale_color_brewer(palette = "Accent")
```

![](DonorsChoose_EDA_Solution_files/figure-html/unnamed-chunk-1-1.png)<!-- -->



```r
teachers$days <- as.integer(strptime(Sys.Date(),"%Y-%m-%d")-
                                strptime(teachers$`Teacher First Project Posted Date`,"%Y-%m-%d"))
teachers_by_days <- as.data.frame(table(teachers$days))
names(teachers_by_days) <- c("Days","Amount")
ggplot(teachers_by_days, aes(x = as.numeric(Days), y = Amount)) + 
    geom_area(colour = "lightgreen", alpha = 0.2)
```

![](DonorsChoose_EDA_Solution_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
