# create an empty plot with text annotation when there is not data for a parameter.
empty_plot<-list(plotly::plot_ly(data=data.frame(x=1,y=1,
                                                text="There is no data for this parameter"),
                                x=~x,
                                y=~y,
                                type="scatter",
                                mode="text",
                                text=~text,
                                textposition="middle")
                
)