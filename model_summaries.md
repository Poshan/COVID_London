# COVID-19 modelling results

## Models fitting comparision

| Model no. | Model 1 | Model 2 | Model 3 |
| -------- | -------- | -------- | ------- |
| formula     | as.integer(caserate) ~ 1 +  f(week, model = "rw2", constr = TRUE) +  f(new_id, model="bym2", graph = msoa.graph)     | as.integer(caserate) ~ 1 + weekly_mean_activity + f(week, model = "rw2", constr = TRUE) +  f(new_id, model="bym2", graph = msoa.graph)      |    as.integer(caserate) ~ 1+ weekly_mean_activity +     MobilityLag + f(week, model = "rw2", constr = TRUE) + f(new_id, model="bym2", graph = msoa.graph)     |
| WAIC | 361854.22 | 361722.03 | 361681.89 |
| CPO     | -193293.4     | -193003    | -192915.8 |
| -------- | -------- | -------- | ------- |
| R2 | 0.93 | 0.93 | 0.93 |
| RMSE     | 41.69     | 41.69     |   41.66  |
| -------- | -------- | -------- | ------- |
| Precision for week | (11.729 to 28.756 ) | (11.696 to 28.671) |(11.614 to 28.505)|
| Precision for area    | (7.436 to 9.798) | (7.002 to 9.220) | (6.217 to 8.255)|
| Phi for area | (0.608 to 0.805) | (0.633 to 0.822) | (0.621 to 0.815) |
| -------- | -------- | -------- | ------- |
| Intercept| (-4.936 to -4.91) | (-5.125 to -5.084) |(-5.150 to -5.107)|
| Activity Within | XX | (0.762 to 0.909) |(0.645 to 0.802)|
| Activity lagged by mobility | XX     | XX     |  (0.388 to 0.647) |



## Residuals in models
### Model 1

![](https://radosgw.public.os.wwu.de/pad/uploads/4ae8143e-ee29-4b8e-b642-5f56a953ab2e.png)


### Model 2
![](https://radosgw.public.os.wwu.de/pad/uploads/719216ff-c072-47bb-8854-c0c86fedc433.png)


### Model 3
![](https://radosgw.public.os.wwu.de/pad/uploads/7bb19e68-c561-42af-ade9-a9287f7c0b48.png)



## Spatial Random effect plots
### Model 1
![](https://radosgw.public.os.wwu.de/pad/uploads/ad72d479-1c1c-45aa-a191-c21060f0d5c6.png)

### Model 2
![](https://radosgw.public.os.wwu.de/pad/uploads/4af9e21a-53cb-4661-a819-608e2b6ffacb.png)

### Model 3
![](https://radosgw.public.os.wwu.de/pad/uploads/24050158-37f5-4237-a72e-2298865e4d4d.png)

## Temporal Random effect plots 
### Model 1
![](https://radosgw.public.os.wwu.de/pad/uploads/0b5b2c65-583f-483f-8d40-31ac53924ee5.png)

### Model 2
![](https://radosgw.public.os.wwu.de/pad/uploads/5c95eaa0-4933-432b-a5e5-7fb0cb084eab.png)

### Model 3
![](https://radosgw.public.os.wwu.de/pad/uploads/3fa65b7b-112d-4d97-b833-0128eafaad37.png)


## Observed vs Fitted caserate
### Temporal plot of selected areas

![](https://radosgw.public.os.wwu.de/pad/uploads/97af3b8e-c121-4f84-b885-b4280dba578b.png)

![](https://radosgw.public.os.wwu.de/pad/uploads/e8ab7b8f-c0d7-4ffb-96e4-87b0703c207f.png)

![](https://radosgw.public.os.wwu.de/pad/uploads/4056a8c9-4844-436e-9abf-518f3332f742.png)

![](https://radosgw.public.os.wwu.de/pad/uploads/af243bae-1e41-4aad-8458-b22ae592b38d.png)

### Spatial plots for selected dates
![](https://radosgw.public.os.wwu.de/pad/uploads/7763819f-ae83-42c4-b763-e38826bf5621.png)

![](https://radosgw.public.os.wwu.de/pad/uploads/d2937f1d-17ad-4f6c-9a8d-464f9a58a47b.png)

![](https://radosgw.public.os.wwu.de/pad/uploads/7a26ebb5-4d35-461c-9189-a20fdf49f284.png)