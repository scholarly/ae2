data = as.data.frame(
  matrix(c(
    0,0,1,1,
    1,0,0,1,
    0,1,0,1,
    0,0,1,0,
    1,0,0,0,
    0,1,0,0
  ),byrow=TRUE,nrow=6)
)
colnames(data) = c("foo", "bar", "baz", "go")

fvars = setdiff(colnames(data),"go")


## This does not work
# sapply(fvars,function(x){
#   data[data[,x]==1,"var"]=x
# })
# apply functions cannot have side effects

## but this does
for(x in fvars){
  data[data[,x]==1,"var"]=x
}

data$var = factor(data$var)
data = data[,c("go","var")]



mat <- matrix(c(1,0,0,
                1,0,0,
                0,1,0,
                0,1,0,
                0,0,1), ncol = 3, byrow = TRUE)
colnames(mat) <- LETTERS[1:3]
mat
as.factor(colnames(mat)[mat %*% 1:ncol(mat)])



# the other direction
mm = model.matrix( ~ Species - 1, data=iris )

# and back again
inames = levels(iris$Species)
#inames = substr(colnames(mm),8,80)
newspec = as.factor(inames[mm %*% 1:ncol(mm)])
identical(newspec,iris$Species)

as.vector(outer(outer(LETTERS[1:5],1:5,FUN=paste0),tolower(LETTERS[20:26]),FUN=paste0))

fo = function(x){x^2}
do.call(fo,.(1:5))
getS3method("median","default")
