#fPoints.R

pointers = as.data.frame(matrix(data = c(4,25	,4,26	,4,27	,4,28	,4,29	,4,30	,4,31	,4,32,5,25,	
5,26,	5,27,	5,28,	5,29,	5,30,	5,31,	5,32,6,25,	6,26,						7,25,	7,26,						8,25,	
8,26,						9,25,	9,26,	9,27,	9,28,	9,29,	9,30,		10,25,	10,26	,10,27	,10,28	,10,29	,
10,30	,	11,25	,11,26	,					12,25	,12,26	,					13,25	,13,26	,					14,25	,14,26	,					
15,25	,15,26	,					16,25	,16,26	,					17,25	,17,26	,					18,25	,18,26	,					
19,25	,19,26	,					20,25	,20,26), ncol = 2, byrow = T))

colnames(pointers) = c("x","y")
pointers$y = pointers$y - 30
pointers$x = pointers$x  - 14
pointers = pointers / 4
original = addKeyPoints(pointers)
