## Reads screencap.png and formats it so that ggplot can plot it below
sc2pv <- function() {
  img <- png::readPNG("screencap.png")
  nr <- nrow(img)
  nc <- ncol(img)
  grd <- expand.grid(1:nr,1:nc)
  dim(img) <- c(nr*nc,4)
  img <- cbind(grd,as.data.frame(img))
  colnames(img) <- c("X","Y","R","G","B","T")
  img$RGB <- with(img,rgb(R,G,B,T))

  p <- ggplot2::ggplot(data=img,mapping=ggplot2::aes(x=Y,y=X,fill=RGB)) + 
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio=nr/nc)

  png("preview.png",width=1000,height=500,units="px",
      bg="transparent",res=216)
  print(p)
  dev.off()
}
