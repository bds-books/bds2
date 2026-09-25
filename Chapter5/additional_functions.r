###################################
# Function to compute all the possible
# k-tuples that can be composed
# based on given set of players
###################################
lineups <- function(dts, players_set, k) {
  
  # Line-ups
  ktuples <- t(combn(players_set,k))
  # Variables names containing player names
  vars_ply_nm <- c(paste0("h",1:5), paste0("a",1:5))
  plyrs <- dts[, vars_ply_nm]
  
  nr <- MIN <- rep(NA, nrow(ktuples))
  for (cnt in 1:nrow(ktuples)) {
    cat(cnt, "of", nrow(ktuples),"\n")
    lineupk <- ktuples[cnt, ]
    filtk <- apply(plyrs, 1, function(x) {
      all(lineupk %in% x) 
    })
    nr[cnt] <- sum(filtk)
    MIN[cnt] <- sum(dts$playlength[filtk])/60
  }
  
  dts_ktuples <- data.frame(player=ktuples, nr=unlist(nr), MIN=unlist(MIN))
  set_plyrs <- sapply(dts_ktuples[, paste0("player.",1:k)], function(x) gsub("(\\b[A-Z])[^A-Z]+", "\\1", x))
  dts_ktuples$label <- apply(set_plyrs, 1, function(x) paste(x,collapse="+"))
  
  return(dts_ktuples)
}

###################################
# Function to compute all the possible
# k-tuples that can be composed
# based on given set of players
# (with parallel computing)
###################################
parlineups <- function(dts, players_set, k, free_cores=2) {
  
  # Line-ups
  ktuples <- t(combn(players_set,k))
  npieces <- 4*(detectCores()-free_cores)
  rows_per_piece <- ceiling(nrow(ktuples) / npieces)
  
  # Split ktuples into npieces pieces by row
  mat_pieces <- lapply(seq(1, nrow(ktuples), by = rows_per_piece), function(i) {
    ktuples[i:min(i+rows_per_piece-1, nrow(ktuples)), ]
  })
  
  # Variables names containing player names
  vars_ply_nm <- paste0(c("a","h"), rep(1:5,each=2))
  plyrs <- dts[, vars_ply_nm]
  playln <- dts$playlength
  
  lnups <- function(ktuples, dts, k) {
    vars_ply_nm <- c(paste0("h",1:5), paste0("a",1:5))
    plyrs <- dts[, vars_ply_nm]
    nr <- MIN <- rep(NA, nrow(ktuples))
    for (cnt in 1:nrow(ktuples)) {
      #cat(cnt, "of", nrow(ktuples),"\n")
      lineupk <- ktuples[cnt, ]
      filtk <- apply(plyrs, 1, function(x) {
        all(lineupk %in% x) 
      })
      nr[cnt] <- sum(filtk)
      MIN[cnt] <- sum(dts$playlength[filtk])/60
    }
    dts_ktuples <- data.frame(player=ktuples, nr=unlist(nr), MIN=unlist(MIN))
    set_plyrs <- sapply(dts_ktuples[, paste0("player.",1:k)], function(x) gsub("(\\b[A-Z])[^A-Z]+", "\\1", x))
    dts_ktuples$label <- apply(set_plyrs, 1, function(x) paste(x, collapse="+")) 
    return(dts_ktuples)
  }
  
  cl <- makeCluster(npieces/4)
  results <- pblapply(X=mat_pieces, FUN=lnups, cl=cl, dts=dts, k=k)
  stopCluster(cl)
  results <- do.call(rbind, results)
  return(results)
}

###################################
# Function to compute
# Adjusted Offensive and Defensive Ratings
###################################
AdjRtg <- function(subdf1) {
  subdf1 <- subdf1 %>%
    mutate(home_r=(home_score/home_poss),
           away_r=(away_score/away_poss))
  team_names <- sort(unique(subdf1$home_team))
  nr <- length(team_names)
  mtx_pts <- mtx_poss <- matrix(NA, nrow=nr, ncol=nr)
  rownames(mtx_pts) <- colnames(mtx_pts) <- 
    rownames(mtx_poss) <- colnames(mtx_poss) <- team_names
  for (i in 1:(nr-1)) {
    rt <- team_names[i]
    for (j in (i+1):nr) {
      ct <- team_names[j]
      rtct <- (subdf1$home_team==rt & subdf1$away_team==ct)
      ctrt <- (subdf1$home_team==ct & subdf1$away_team==rt)
      n <- sum(rtct | ctrt)
      if (n>0) {       
        times  <-  c( subdf1$game_date[rtct],  subdf1$game_date[ctrt])
        ij_pts  <- c(subdf1$home_score[rtct], subdf1$away_score[ctrt])
        ji_pts  <- c(subdf1$away_score[rtct], subdf1$home_score[ctrt])
        ij_poss <- c( subdf1$home_poss[rtct],  subdf1$away_poss[ctrt])
        ji_poss <- c( subdf1$away_poss[rtct],  subdf1$home_poss[ctrt])		
        mtx_pts[i, j] <- sum(ij_pts)
        mtx_pts[j, i] <- sum(ji_pts)
        mtx_poss[i, j] <- sum(ij_poss)
        mtx_poss[j, i] <- sum(ji_poss)		
      }
    }
  }
  
  mtx_r <- mtx_pts/mtx_poss
  mn_by_col <- apply(mtx_pts, 2, sum, na.rm=T)/apply(mtx_poss, 2, sum, na.rm=T)
  AORtg <- apply(t(t(mtx_r)-mn_by_col), 1, mean, na.rm=T)
  
  mn_by_row <- apply(mtx_pts, 1, sum, na.rm=T)/apply(mtx_poss, 1, sum, na.rm=T)
  ADRtg <- apply(mtx_r-mn_by_row, 2, mean, na.rm=T)  

  out <- data.frame(AORtg=AORtg, ADRtg=-ADRtg)
  #out <- data.frame(AORtg=mn_by_row, ADRtg=-mn_by_col)
  return(out)
}


###################################
# set up starting from some codes by Ed Küpfer (GitHub)
# https://gist.github.com/edkupfer/6354964
# Function to plot the NBA court
###################################
plot_court <- function(linewidth=.75) {
list(
    ###outside box:
    geom_path(data=data.frame(y=c(-25,-25,25,25,-25),x=c(-47,47,47,-47,-47)), aes(x=x,y=y), linewidth=linewidth),
    ###halfcourt line:
    geom_path(data=data.frame(y=c(-25,25),x=c(0,0)), aes(x=x,y=y), linewidth=linewidth),
    ###halfcourt semicircle:
    geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y), linewidth=linewidth),
    ###solid FT semicircle above FT line:
    geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y), linewidth=linewidth),
    ###dashed FT semicircle below FT line:
    geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed', linewidth=linewidth),
    geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed', linewidth=linewidth),
    ###key:
    geom_path(data=data.frame(y=c(-8,-8,8,8,-8),x=c(47,28,28,47,47)), aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=-c(-8,-8,8,8,-8),x=-c(47,28,28,47,47)), aes(x=x,y=y), linewidth=linewidth),
    ###box inside the key:
    geom_path(data=data.frame(y=c(-6,-6,6,6,-6),x=c(47,28,28,47,47)), aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=c(-6,-6,6,6,-6),x=-c(47,28,28,47,47)), aes(x=x,y=y), linewidth=linewidth),
    ###restricted area semicircle:
    geom_path(data=data.frame(y=c(-4000:(-1)/1000,1:4000/1000),x=c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=c(-4000:(-1)/1000,1:4000/1000),x=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y), linewidth=linewidth),
    ###rim:
    geom_path(data=data.frame(y=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),x=c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),x=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y), linewidth=linewidth),
    ###backboard:
    geom_path(data=data.frame(y=c(-3,3),x=c(43,43)), aes(x=x,y=y), lineend='butt', linewidth=linewidth),
    geom_path(data=data.frame(y=c(-3,3),x=-c(43,43)), aes(x=x,y=y), lineend='butt', linewidth=linewidth),
    ###three-point line:
    geom_path(data=data.frame(y=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),x=c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y), linewidth=linewidth),
    geom_path(data=data.frame(y=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),x=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y), linewidth=linewidth)
)
}

#################################################################################
# Adds a legend on the right side of the court 
#################################################################################

add_legend <- function(data) {
  list(
  geom_point(data=data,
             aes(x=x, y=y, color=group2),
             size=8, shape=21, stroke=1, inherit.aes=FALSE, show.legend=FALSE),
  geom_text(data=data,
            aes(x=x, y=y, color=group2, label=num),
            size=4, inherit.aes=FALSE, show.legend=FALSE),
  geom_text(data=data,
            aes(x=x, y=y, color=group2, label=name),
            hjust="left", nudge_x=1.8, size=4,
            inherit.aes=FALSE, show.legend=FALSE) )
}


#################################################################################
# Converts the game clock, expressed as seconds elapsed from the beginning of a 
# quarter, into a minutes:seconds.decimal format showing the time remaining 
# in the quarter
#################################################################################
sec_to_clock <- function(seconds) {
  min <- floor(seconds / 60)
  sec <- seconds %% 60
  sprintf("%02d:%04.1f", min, sec)
}

###################################
# Function to determine convex hull areas
# of offensive and defensive team
# and their intersection
###################################
chulls <- function(data) {
  subdts <- data %>% filter(name!="Ballcarrier" & name!="Ball")
  tm <- sort(unique(subdts$time))
  n <- length(tm)
  dts_chull <- lapply(seq_along(tm), function(k) {
    if (k %% round(n / 10) == 0) {
      cat("Reached", round((k / n) * 100), "% of the loop\n")
    }
    t <- tm[k]
    df_h <- subdts %>% filter(time==t & group2=="home") %>% select(x,y)
    chull_h <- chull(df_h)
    chull_h <- c(chull_h, chull_h[1]) 
    poly_h <- st_polygon(list(as.matrix(df_h[chull_h,])))
    df_h$area <- st_area(poly_h)
    df_h$group2 <- "home"
    df_a <- subdts %>% filter(time==t & group2=="away") %>% select(x,y)
    chull_a <- chull(df_a)
    chull_a <- c(chull_a, chull_a[1])
    poly_a <- st_polygon(list(as.matrix(df_a[chull_a,])))
    df_a$area <- st_area(poly_a)
    df_a$group2 <- "away"
    df_ha <- rbind(df_h[chull_h, ], df_a[chull_a, ])
    inters <- st_intersection(st_sfc(poly_h), st_sfc(poly_a))
    if (length(inters)>0) {
      df_ha$inters_area <- st_area(inters)
    } else {
      df_ha$inters_area <- 0
    }
    df_ha$time <- t
    return(df_ha)
  })
  dts_chull <- do.call(rbind, dts_chull)
  dts_chull$group2 <- factor(dts_chull$group2, levels=c("home","away"))
  dts_chull <- left_join(dts_chull, 
                         subdts %>% group_by(time) %>% summarize(off_team=first(off_team)), 
                         by="time")
  area_chull_off <- dts_chull %>% filter(off_team==group2) %>% group_by(time) %>% 
    summarize(o_area=first(area), o_x=mean(x), o_y=mean(y), o_group2=first(group2), inters_area=first(inters_area)) %>% 
    ungroup() %>% as.data.frame()
  area_chull_def <- dts_chull %>% filter(off_team!=group2) %>% group_by(time) %>% 
    summarize(d_area=first(area), d_x=mean(x), d_y=mean(y), d_group2=first(group2)) %>% 
    ungroup() %>% as.data.frame()
  area_chull_od <- left_join(area_chull_off, area_chull_def, by="time")
  return(list(area_chull_od=area_chull_od, dts_chull=dts_chull))
}

#####################
# Calculation of barycentric coordinates
#####################
bary_coord <- function(xA, xB, xC, xK, yA, yB, yC, yK) {
  wAn <- (yB-yC)*(xK-xC)+(xC-xB)*(yK-yC)
  wBn <- (yC-yA)*(xK-xC)+(xA-xC)*(yK-yC)
  wd <- (yB-yC)*(xA-xC)+(xC-xB)*(yA-yC)
  wA <- wAn/wd
  wB <- wBn/wd
  wC <- 1 - wA - wB
  wA[is.infinite(wA)] <- NA
  wB[is.infinite(wB)] <- NA
  wC[is.infinite(wC)] <- NA
  return(cbind(wA,wB,wC))
}


###################################
# Function that enables the visualization of a single 
# gameplay instance and (optionally) the gravity and 
# distraction for a given player
###################################
plot_single_frame <- function(data_long, data_wide, time_sf, GD=FALSE, player_name, player_legend=TRUE) {
  
  data_long_sf <- data_long %>% 
    filter(time==time_sf) %>%
    mutate(pl_num=ifelse(group1=="ball" | group1=="bcarr", NA, 
                         as.numeric(gsub("[^0-9]", "", group1)))) %>%
    mutate(pl_num = ifelse(is.na(pl_num),"",pl_num))
  
  p1 <- ggplot() + plot_court() +
    geom_point(aes(x=x, y=y, group=group1, color=group2, shape=group2, size=group2), data=data_long_sf, stroke=1) +
    geom_text(aes(x=x, y=y, label=pl_num, color=group2), data=data_long_sf, size=6, show.legend=F) +
    geom_text(aes(x=0, y=Inf, label=paste("Quarter:", quarter, " - Game clock:", sec_to_clock(game_clock))), 
              data=data_long_sf, hjust=0.5, vjust=1, size=5) +
    scale_color_manual(values=c("away"="blue","home"="red","ball"="orange","bcarr"="orange")) +
    scale_shape_manual(values=c("away"=21,"home"=21,"ball"=19,"bcarr"=23)) +
    scale_size_manual(values=c("away"=8,"home"=8,"ball"=6,"bcarr"=8)) + 
    labs(color="", shape="", size="") +
    theme_void() %+replace% theme(legend.position="bottom")
  
  if (player_legend) {


  data_long_txt <- data_long_sf %>%  filter(group1!="ball" & group1!="bcarr") 
  
  data_long_leg <- data_long_txt %>%
    group_by(name) %>%
    summarize(num=as.numeric(substr(first(group1),2,2)), 
              ah=substr(first(group1),1,1),
              group2 = ifelse(ah == "a", "away", "home")) %>%
    arrange(ah, -num) %>%
    mutate(x = 50, y = seq(-23,23,length.out=10))

    p1 <- p1 + xlim(c(-47,65)) + add_legend(data_long_leg)
  } 

  
  if (GD) {					    
    data_wide_sf <- data_wide %>% filter(time==time_sf) 
    oppDist <- opponentDist(data_wide_sf, name=player_name)
    nr <- nrow(oppDist$xyOpp)
    qrt <- unique(data_wide_sf$quarter)
    if (oppDist$isHome) {
      if (qrt==1 | qrt==2) bsk_x <- -41.75 else bsk_x <- 41.75
      plr_col <- "red"; opp_col <- "blue"
    } else {
      if (qrt==1 | qrt==2) bsk_x <- 41.75 else bsk_x <- -41.75
      plr_col <- "blue"; opp_col <- "red"
    }
    
    data_wide_segm <- with(oppDist, data.frame(time=data_wide_sf$time,
                                               quarter=data_wide_sf$quarter,
                                               oppx=xyOpp[,1], oppy=xyOpp[,2],
                                               bcarrx=data_wide_sf$bcarr_x,
                                               bcarry=data_wide_sf$bcarr_y, 
                                               plyrx=xyPlyr[,1], plyry=xyPlyr[,2],
                                               bskx =rep(bsk_x, nr), bsky=rep(0, nr) ))
    
    barycentric_coord <- with(data_wide_segm, bary_coord(bcarrx, plyrx, bskx, oppx, 
                                                         bcarry, plyry, bsky, oppy))
    data_wide_segm <- cbind(data_wide_segm, barycentric_coord)
    
    data_wide_poly <- with(data_wide_segm, data.frame(x=c(rep(bsk_x, nrow(data_wide_segm)), plyrx, bcarrx), 
                                                      y=c(rep(0, nrow(data_wide_segm)), plyry, bcarry),
                                                      time=rep(time, 3) ))
    
    p1 <- p1 +
      geom_segment(data=data_wide_segm, aes(x=oppx, y=oppy, xend=plyrx, yend=plyry, group=time), linewidth=1, color=plr_col, alpha=0.5) +
      geom_polygon(data=data_wide_poly, aes(x=x, y=y, group=time), fill=opp_col, alpha=0.2, color=opp_col) +
      geom_text(data=data_wide_segm, aes(x=bcarrx, y=bcarry, label=round(wA,2)), size=4.5, hjust=1, vjust=1) +
      geom_text(data=data_wide_segm, aes(x=plyrx, y=plyry, label=round(wB,2)), size=4.5, hjust=1, vjust=1) +
      geom_text(data=data_wide_segm, aes(x=bskx, y=bsky, label=round(wC,2)), size=4.5, hjust=1, vjust=1) 
  }
  
  invisible(p1)
}


###################################
# Function to determine the distance
# between a player and the nearest
# opposing team player
###################################
opponentDist <- function(data, name) {
  h_names <- data[, paste0("h", 1:5, "_name")]
  h_X     <- data[, paste0("h", 1:5, "_x")]
  h_Y     <- data[, paste0("h", 1:5, "_y")] 
  a_names <- data[, paste0("a", 1:5, "_name")]
  a_X     <- data[, paste0("a", 1:5, "_x")]
  a_Y     <- data[, paste0("a", 1:5, "_y")]
  isHome <- name %in% unique(unlist(h_names))
  if (isHome) {
    namesMtx <- h_names
    plMtxX  <- h_X; plMtxY  <- h_Y
    oppMtxX <- a_X; oppMtxY <- a_Y
  } else {
    namesMtx <- a_names
    plMtxX  <- a_X; plMtxY  <- a_Y
    oppMtxX <- h_X; oppMtxY <- h_Y
  }
  filt_plyr <- apply(namesMtx, 2, function(y) y==name)
  x <- plMtxX[filt_plyr]; y <- plMtxY[filt_plyr]
  dstMtx <- lapply(1:5, function(k) {
    sqrt((x-oppMtxX[, k])^2+(y-oppMtxY[, k])^2)   
  })
  dstMtx <- do.call(cbind, dstMtx)
  ordMtx <- t(apply(dstMtx, 1, order))
  if (isHome) {X <- a_X; Y <- a_Y} else {X <- h_X; Y <- h_Y}
  xyOpp <- lapply(1:nrow(X), function(k) {
    c(X[k, ordMtx[k, 1]], Y[k, ordMtx[k, 1]])
  })
  xyOpp <- as.data.frame(t(do.call(cbind, xyOpp)))
  xyPlyr  <- data.frame(x=x, y=y); 
  rownames(xyOpp) <- rownames(xyPlyr) <- NULL
  names(xyOpp) <- names(xyPlyr) <- c("x", "y")
  return(list(isHome=isHome, dstMtx=dstMtx, ordMtx=ordMtx, 
              xyOpp=xyOpp, xyPlyr=xyPlyr))
}

###################################
# Function to determine if a player
# is within a given width band
# around the 3-point line
###################################
between_3pt_line <- function(x, y, delta=5, lr="right") {
  sgn <- ifelse(lr=="left", -1, 1)
  r_lo <- 23.75-delta
  r_up <- 23.75+delta
  tst <- TRUE
  if (lr=="right") {
    if (y > -(22-delta) & y < 22-delta) {
      if (x > sgn*(41.75-sqrt(r_lo^2 - y^2))) tst <- FALSE
    } 
    if (y > -(22+delta) & y < 22+delta) {
      if (x < sgn*(41.75-sqrt(r_up^2 - y^2))) tst <- FALSE
    }
  } else if (lr=="left") {
    if (y > -(22-delta) & y < 22-delta) {
      if (x < -(41.75-sqrt(r_lo^2 - y^2))) tst <- FALSE
    } 
    if (y > -(22+delta) & y < 22+delta) {
      if (x > -(41.75-sqrt(r_up^2 - y^2))) tst <- FALSE
    }
  }
  
  if (y <= -(22+delta) | y >= 22+delta) {
    tst <- FALSE
  }
  return(tst)
}
between_3pt_line <- Vectorize(between_3pt_line, vectorize.args=c("x","y"))


####################
# Determine if the player is in the home or away team.
####################
is_home <- function(data, name) {
  h_names <- data[, paste0("h", 1:5, "_name")]
  is_home <- name %in% unique(unlist(h_names))
  return(is_home)
}

####################
# Select the game moments in which the player (player) is present, 
# based on the possession number (nposs) and 
# whether the player is within the area around the three-point line 
# (+/- delta is the width of area)
####################
filt_rule <- function(data, npossk, namek, delta=5) {
  datak <- data %>% filter(nposs==npossk)
  qrt <- unique(datak$quarter)
  #tbl <- table(qrt)
  #if (length(tbl)>1) print(npossk)
  qrt <- qrt[1]
  isHome <- is_home(datak , namek)
  if (isHome) {
    ### Filtra le sole azioni in attacco
    datak <- datak %>% filter(off_team=="home")
    if (length(unlist(datak))==0) return(NULL)
    ### Trova coordinate giocatore
    h_X <- datak[, paste0("h", 1:5, "_x")]
    h_Y <- datak[, paste0("h", 1:5, "_y")] 
    h_names <- datak[, paste0("h", 1:5, "_name")] 
    filt_plyr <- apply(h_names, 2, function(y) y==namek)
    filt_rows <- apply(filt_plyr, 1, any)
    datak <- datak[filt_rows, ]
    if (length(unlist(datak))==0) return(NULL)
    x <- h_X[filt_plyr]
    y <- h_Y[filt_plyr]
    ### Considera solo i momenti in cui il player è 
    ### dentro la fascia intorno la linea dei 3 pt
    if(qrt==1 | qrt==2) lr <- "left" else lr <- "right"
    filt_pt3 <- between_3pt_line(x, y, delta=delta, lr=lr)
    datak <- datak[filt_pt3, ]
    if (length(unlist(datak))==0) return(NULL)
    x <- x[filt_pt3]
    y <- y[filt_pt3]
  } else {
    ### Filtra le sole azioni in attacco
    datak <- datak %>% filter(off_team=="away")
    if (length(unlist(datak))==0) return(NULL)
    ### Trova coordinate giocatore
    a_X <- datak[, paste0("a", 1:5, "_x")]
    a_Y <- datak[, paste0("a", 1:5, "_y")] 
    a_names <- datak[, paste0("a", 1:5, "_name")] 
    filt_plyr <- apply(a_names, 2, function(y) y==namek)
    filt_rows <- apply(filt_plyr, 1, any)
    datak <- datak[filt_rows, ]
    if (length(unlist(datak))==0) return(NULL)
    x <- a_X[filt_plyr]
    y <- a_Y[filt_plyr]
    ### Considera solo i momenti in cui il player è 
    ### dentro la fascia intorno la linea dei 3 pt
    if(qrt==1 | qrt==2) lr <- "right" else lr <- "left"
    filt_pt3 <- between_3pt_line(x, y, delta=delta, lr=lr)
    datak <- datak[filt_pt3, ]
    if (length(unlist(datak))==0) return(NULL)
    x <- x[filt_pt3]
    y <- y[filt_pt3]
  }
  datak$x <- x; datak$y <- y
  return(datak)
}

####################
# Calculates gravity for a given player (player) at 
# a given possession number (nposs)
####################
plyr_nposs_gravity <- function(data_wide, nposs, player, delta=5, min_n=10) {
  datak <- filt_rule(data_wide, nposs, player, delta=delta)
  if (is.null(datak)) {
    gravity <- NULL
  } else if (nrow(datak) < min_n) {
    gravity <- NULL
  } else {      
    oppDistk <- opponentDist(datak, player)     
    qrt <- unique(datak$quarter)
    if (oppDistk$isHome) {
      if (qrt==1 | qrt==2) bsk_x <- -41.75 else bsk_x <- 41.75
    } else {
      if (qrt==1 | qrt==2) bsk_x <- 41.75 else bsk_x <- -41.75
    }
    nr <- nrow(oppDistk$xyOpp)
    dfk <- with(oppDistk, data.frame(time=datak$time,
                                     nposs=datak$nposs,
                                     oppx=xyOpp[,1], oppy=xyOpp[,2],
                                     bcarrx=datak$bcarr_x, bcarry=datak$bcarr_y, 
                                     bcarrName=datak$bcarr_name,
                                     plyrx=xyPlyr[,1], plyry=xyPlyr[,2],
                                     bskx=rep(bsk_x, nr), bsky=rep(0, nr) ))
    barycentric_coord <- with(dfk, bary_coord(bcarrx, plyrx, bskx, oppx, 
                                              bcarry, plyry, bsky, oppy))
    dfk <- cbind(dfk, barycentric_coord)
    dfk$name <- player
    gravity <- dfk
  }
  return(gravity)
}

####################
# Calculates gravity for a given player (player)
# at a given set of possession numbers (np_set) 
####################
plyr_gravity <- function(data_wide, player, np_set, delta=5, min_n=10) {
  cat("\n--- Calculating gravity for", player, "---\n")
  gravity <- lapply(np_set, function(npk) {
    plyr_nposs_gravity(data_wide, npk, player, delta=5, min_n=min_n)
  })
  gravity <- do.call(rbind, gravity)
  return(gravity)
}

###################
# This function loads the image from the specified file and 
# then processes it to be ready as input for the MoveNet CNN
####################
preprocess_image <- function(file, resize, dtype, crop=NULL) {
  image0 <- tf$io$read_file(file)
  image0 <- tf$compat$v2$image$decode_image(image0)
  if (!is.null(crop)) {
    offset_height <- crop[1]
     offset_width <- crop[2]
             hbox <- crop[3]
             wbox <- crop[4]
    image0 <- tf$image$crop_to_bounding_box(image0, offset_height, offset_width, hbox, wbox)
  }
  image1 <- tf$expand_dims(image0, axis=0L)
  image1 <- tf$image$resize_with_pad(image1, as.integer(resize[1]), as.integer(resize[2]))
  image1 <- tf$cast(image1, dtype=dtype)
  return(list(image_raw=image0, image_proc=image1))
}

###################
# This function takes the input image and returns the prediction provided by 
# the pre-trained (singlepose) Movenet model specified with the "interpreter" argument
####################
movenet_singlepose_predict <- function(interpreter, input_image) {
  input_details <- interpreter$get_input_details()
  interpreter$allocate_tensors()
  interpreter$set_tensor(input_details[[1]][['index']], input_image)
  interpreter$invoke()
  output_details <- interpreter$get_output_details()
  pred_keypts <- interpreter$get_tensor(output_details[[1]][['index']])
  return(pred_keypts)
}

###################
# This function takes the input image and returns the prediction provided by 
# the pre-trained (multipose) Movenet model specified with the "interpreter" argument.
####################
movenet_multipose_predict <- function(interpreter, input_image) {
  input_details <- interpreter$get_input_details()
  input_shape <- input_image$shape
  interpreter$resize_tensor_input(input_details[[1]][['index']], input_shape, strict=TRUE)
  interpreter$allocate_tensors()
  interpreter$set_tensor(input_details[[1]][['index']], input_image)
  interpreter$invoke()
  output_details <- interpreter$get_output_details()
  idx_out <- output_details[[1]][['index']]
  pred_keypts <- interpreter$get_tensor(idx_out)
  return(pred_keypts)
}


###################
# Based on the output provided by a MoveNet singlepose, 
# this function provides the coordinates of the keypoints (with the score) and 
# the coordinates of the endpoints of the edges
####################
movenet_singlepose_features <- function(keypoints_with_scores, labs=NULL, score_threshold=0.10,  
                       col_edges_left="yellowgreen", col_edges_right="peachpuff", col_edges_horiz="royalblue") {
  # Returns high confidence keypoints and edges for visualization.
  
  edges_col <- 
    data.frame(ind1=c(1,1,2,3,1,1,6,8,7,9,6,6,7,12,12,14,13,15),
               ind2=c(2,3,4,5,6,7,8,10,9,11,7,12,13,13,14,16,15,17),
               col=c(col_edges_right, col_edges_left, col_edges_right, col_edges_left,
			         col_edges_right, col_edges_left, col_edges_right, col_edges_right,
					 col_edges_left, col_edges_left, col_edges_horiz, col_edges_right,
					 col_edges_left, col_edges_horiz, col_edges_right, col_edges_right,
					 col_edges_left, col_edges_left))
  
  kpts_x <- keypoints_with_scores[1, 1, , 2]
  kpts_y <- 1 - keypoints_with_scores[1, 1, , 1]
  kpts_scores <- keypoints_with_scores[1, 1, , 3] 
  filt <- (kpts_scores > score_threshold)
  if (sum(filt)>0) {
    kpts_absolute_xy <- data.frame(x=as.numeric(kpts_x), 
                                   y=as.numeric(kpts_y), 
                                   score=kpts_scores, labs=labs)
    keypoints_all <- kpts_absolute_xy[filt, ]
  } else {
    keypoints_all <- NULL
  }
  
  edges_all <- list() 
  for (k in 1:nrow(edges_col)) {
    edge_pair1 <- edges_col[k, 1]
    edge_pair2 <- edges_col[k, 2]
    color <- edges_col[k, 3]
    if ((kpts_scores[edge_pair1] > score_threshold) & (kpts_scores[edge_pair2] > score_threshold)) {
      x_start <- kpts_absolute_xy[edge_pair1, 1]
      y_start <- kpts_absolute_xy[edge_pair1, 2]
      x_end   <- kpts_absolute_xy[edge_pair2, 1]
      y_end   <- kpts_absolute_xy[edge_pair2, 2]
      line_seg <- data.frame(x_start=x_start, y_start=y_start, x_end=x_end, y_end=y_end, color=color, edge=k)
      edges_all <- append(edges_all, list(line_seg))
    }
  }
  edges_all <- do.call(rbind, edges_all)
  
  return(list(keypoints=keypoints_all, edges=edges_all))
}


###################
# Based on the output provided by a MoveNet multipose, 
# this function provides the coordinates of the keypoints (with the score), 
# the coordinates of the endpoints of the edges, and 
# the coordinates of the bounding boxes
####################
movenet_multipose_features <- function(pred_keypts, labs=NULL, keypoint_score_cutoff=0.10, bounding_box_score_cutoff=0.10) {
  n_poses <- dim(pred_keypts)[2]
  keypoints_list <- edges_list <- bndbxs_list <- list()
  cnt = 1 
  for (k in 1:n_poses) {
    posek <- pred_keypts[1,k,]
    nk <- length(posek)
    bnd_bx_k  <- posek[(nk-4):nk]
    keypts_scoresk <- matrix(posek[1:(nk-5)], nrow=17, byrow=T)
    keypts_scoresk <- array(keypts_scoresk, dim = c(1, 1, 17, 3))
    keypts_locs_edges <- movenet_singlepose_features(keypts_scoresk, labs=labs,
    score_threshold=keypoint_score_cutoff)  
    keypoints <- keypts_locs_edges[["keypoints"]]
    edges     <- keypts_locs_edges[["edges"]]	
    if (!is.null(keypoints) & !is.null(edges)) {
      keypoints$player <- k 
      keypoints_list   <- append(keypoints_list, list(keypoints))
      
      edges$player <- k
      edges_list   <- append(edges_list, list(edges))
      
      bnd_bx_k   <- data.frame(xmin=bnd_bx_k[2],ymin=1-bnd_bx_k[1],
                               xmax=bnd_bx_k[4],ymax=1-bnd_bx_k[3],
                               score=bnd_bx_k[5], player=k)
      bndbxs_list <- append(bndbxs_list, list(bnd_bx_k))
      
      cnt = cnt + 1 
    }
  }
  keypoints <- do.call(rbind,  keypoints_list)
  edges     <- do.call(rbind,  edges_list)  
  bndbxs   <- do.call(rbind,  bndbxs_list)
  filt_player <- bndbxs[bndbxs$score > bounding_box_score_cutoff, "player"]
  keypoints <- keypoints %>% filter(player %in% filt_player)
  edges <- edges %>% filter(player %in% filt_player)
  bndbxs <- bndbxs %>% filter(player %in% filt_player)

  return(list(keypoints=keypoints, edges=edges, bounding_boxes=bndbxs))
}


###################
# This function plots a stylized 2D model of the human body
# starting from a list of coordinates of key points and edges
# provided by a MoveNet singlepose CNN applied to a sequence
# of frames
####################
animation_keypoints_edges <- function(keypoints, edges, out_filename, out_dir, 
                                 out_format="mp4", tmp_dir=tempdir()) {
  nframes <- length(keypoints)
  plot_list <- vector(nframes, mode="list")
  for (k in 1:nframes) {
    keypoints_k <- keypoints[[k]]
    keypoints_k$n <- 1:nrow(keypoints_k)
    edges_k <- edges[[k]]
    p_k <- ggplot() +
      geom_segment(aes(x=x_start, y=y_start,
                       xend=x_end, yend=y_end),
                   color=edges_k$color, linewidth=1,
                   data=edges_k) +
      geom_point(aes(x=x, y=y, fill=score, color=score),
                 data=keypoints_k, size=3,
                 show.legend=T, pch=21) +
      geom_point(aes(x=x, y=y), data=keypoints_k,
                 color="white", size=1.25,
                 show.legend=T) +
      scale_color_gradientn(name="Keypoint score", limits = c(0,1),
                            colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")) +
      scale_fill_gradientn(name="Keypoint score", limits = c(0,1),
                           colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")) +
      xlim(c(0,1)) + ylim(c(0,1)) + theme_void() +
      theme(legend.position="inside",
            legend.position.inside=c(0.9,0.8),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            plot.background = element_rect(fill="black"))
    plot_list[[k]] <- p_k
    jpeg(file=paste0(tmp_dir,"/frame",k,".jpg"),
         height=2000, width=2000, res=300)
    print(p_k)
    dev.off()
  }
  img_seq <- do.call(c,
     lapply(paste0(tmp_dir,"/frame",c(1:nframes),".jpg"), image_read))
  switch(
     out_format,
     mp4 = image_write_video(image = img_seq, framerate=20,
                path = paste0(out_dir,out_filename,".mp4")),
     gif = image_write_gif(image = img_seq, loop = TRUE, delay = 1/20,
                           path = paste0(out_dir,out_filename,".gif"))
  )
 
}


###################
# This function displays a raster image
# and overlays the associated bounding boxes,
# with line width scaled by the detection confidence. 
# Class labels are rendered on each box
###################
plot_bbox <- function(img, bbox, lwd_range=c(0,1), label=TRUE, legend=FALSE) {
  if (!is.raster(img)) img <- as.raster(img)
  dim_img <- dim(img)
  p <- ggplot() + 
    annotation_custom(rasterGrob(img), 
                      xmin = 0, xmax = dim_img[2], ymin = 0, ymax = dim_img[1]) +
    coord_fixed() +
    xlim(c(0,dim_img[2])) + ylim(c(0,dim_img[1])) +
    theme_void()
  
  if (!is.null(bbox) && nrow(bbox) > 0) {
    bbox$xmid <- (bbox$xmin+bbox$xmax)/2
    
    p  <- p +  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, linewidth=confidence), 
                         data=bbox, color="blue", fill="transparent", show.legend=legend) +
      scale_linewidth_continuous(breaks=seq(0.1, 0.8, 0.1), range=lwd_range) +
      guides(linewidth = guide_legend(override.aes = list(color = "white"))) +
      theme(legend.position="inside", legend.position.inside=c(0.5, 0.09), 
            legend.title=element_text(color="white"), legend.direction="horizontal",
            legend.text=element_text(color="white"))
    
    if (label) {
      p <- p + geom_label(aes(x=xmid, y=max(ymin,ymax), label=class), 
                          hjust = 0.5, vjust = -0.1, col="white", size=2.5, 
                          fill="blue", data=bbox)
    }
  }
  return(p)
}


###################
# Angle between two vectors
####################
angle_between_vectors <- function(A, B, C) {
  v1 <- A - B
  v2 <- C - B
  dot_product <- sum(v1 * v2)  # Scalar product
  mag_v1 <- sqrt(sum(v1^2))   # norm of v1
  mag_v2 <- sqrt(sum(v2^2))   # norm of v2
  cos_theta <- dot_product / (mag_v1 * mag_v2)  # cosine of the angle
  theta <- acos(cos_theta)  # angle in radians
  theta_deg <- theta * (180 / pi)  # conversion to degrees
  return(theta_deg)
}                                   

###################
# Shot Ball Trajectory Analysis
###################
trajectory_analysis <- function(dts, basket=c(0,0,0)) {
  
  ### Fit 3D parabola
  lm_x <- lm(x~time, data=dts)
  coef_x <- coef(lm_x)
  names(coef_x) <- NULL
  dts$x_pred <- predict(lm_x)
  
  lm_y <- lm(y~time, data=dts)
  coef_y <- coef(lm_y)
  names(coef_y) <- NULL
  dts$y_pred <- predict(lm_y)
  
  # Quadratic/parabolic model
  lm_z <- lm(z ~ time + I(time^2), data=dts)
  coef_z <- coef(lm_z)
  names(coef_z) <- NULL
  dts$z_pred <- predict(lm_z)
  dts$resid <- residuals(lm_z)
  
  
  ### Ideal direction and midpoint 
  start_point <- dts[1, c("x_pred","y_pred","z_pred")]
  x0 <- start_point$x_pred; y0 <- start_point$y_pred; z0 <- start_point$z_pred
  ideal_dir <- data.frame(x=c(x0, basket[1]), 
                          y=c(y0, basket[2]), 
                          z=c(z0, basket[3]))
  midpoint <- (start_point+basket)/2
  names(midpoint) <- c("x","y","z")
  
  
  ### Compute additional parabolic trajectory features
  # Initial speed (ft/sec or m/sec)
  isp.feet <- sqrt(coef_x[2]^2+coef_y[2]^2+coef_z[2]^2)
  isp.meters <- isp.feet*0.3048
  
  # Release angle
  angle <- asin(abs(coef_z[2])/isp.feet)*180/pi
  
  ### Distance between midpoint and projected vertex
  # Vertex
  timeV <- -coef_z[2]/(2*coef_z[3])
  xV    <- coef_x[1] + coef_x[2] * timeV
  yV    <- coef_y[1] + coef_y[2] * timeV
  zV    <- -(coef_z[2]^2-4*coef_z[1]*coef_z[3])/(4*coef_z[3])
  
  # Project V onto the ideal direction
  m <- (basket[2] - y0)/(basket[1] - x0)
  xV_proj <- (xV + m * yV - m^2 * basket[1] + m * basket[2])/(1 + m^2)
  yV_proj <- (basket[2] + m * xV + m^2 * yV - m * basket[1])/(1 + m^2)
  tV_proj <- (yV_proj - y0)/(basket[2] - y0)
  zV_proj <- z0 + tV_proj * (basket[3] - z0)
  
  # Distance between the projection of the vertex on the ideal direction and its midpoint.
  xm <- midpoint$x;  ym <- midpoint$y; zm <- midpoint$z
  dist_VvsM <- sqrt((xm - xV_proj)^2 + (ym - yV_proj)^2) 
  dist_m_C    <- sqrt((xm - basket[1])^2 + (ym - basket[2])^2) 
  dist_proj_C <- sqrt((xV_proj - basket[1])^2 + (yV_proj - basket[2])^2)
  sign_V_pos <- ifelse(dist_proj_C < dist_m_C, -1, 1) 
  
  distV  <- data.frame(xV, yV, zV, timeV, 
                       xV_proj, yV_proj, zV_proj, 
                       dist_VvsM, sign_V_pos)
                       
  rownames(distV) <- NULL
  
  ### Directions                 
  # line slope (ideal direction)
  m1 <- (basket[2]-y0)/(basket[1]-x0)
  
  # line slope (actual direction)
  tps <- nrow(dts)
  m2 <- (dts$y_pred[tps]-y0)/(dts$x_pred[tps]-x0)
  
  # Angle between the two lines
  angle_dir <- atan(abs((m2-m1)/(1+m1*m2)))*180/pi
  
  return(list(df_pred=dts, distV=distV, 
              speed_angle=c(isp.feet=isp.feet, isp.meters=isp.meters, angle=angle),
              models=list(mod_x=lm_x, mod_y=lm_y, mod_z=lm_z),
              directions=list(m1=m1, m2=m2, angle_dir=angle_dir, ideal_dir=ideal_dir, midpoint=midpoint) ))
}



###################
# 3D Trajectory Plotting
###################
plot_single_traj <- function(dts, basket=c(0,0,0), speed_dir=FALSE, title=NULL, range=NULL, 
                             trace=FALSE, fig=NULL, scene="scene",
                             traj_col="blue", est_traj_col="tomato", vert_col="lawngreen", 
                             traj_dir_col="blue", ideal_dir_col="orange", speed_dir_col="maroon") {
  
  tps <- nrow(dts)
  range.x <- range.y <- range
  
  # Basket hoop
  xc <- basket[1]; yc <- basket[2]; zc <- basket[3]
  angles <- seq(0, 2*pi, length.out = 100)
  radius <- 0.75
  basket_hoop <-  data.frame(x = xc + radius * cos(angles),
                             y = yc + radius * sin(angles),
                             z = rep(zc, 100) )
  
  # Trajectory analysis
  results <- trajectory_analysis(dts, basket=basket)
  df   <- results[["df_pred"]]
  df_V <- results[["distV"]]
  midpoint  <- results[["directions"]]$midpoint
  ideal_dir <- results[["directions"]]$ideal_dir
  coef_x <- coef(results[["models"]]$mod_x)
  coef_y <- coef(results[["models"]]$mod_y)
  coef_z <- coef(results[["models"]]$mod_z)
  
  # Vertex projection
  if (!trace) {
    fig <- plot_ly()
  } 
  fig <- fig %>% 
    add_trace(x = ~x, y = ~y, z = ~z, data=df, scene=scene,
              type = 'scatter3d', mode = 'lines+markers', inherit=F, showlegend=FALSE,
              line = list(width=4, color=traj_col), marker=list(size = 5)) %>% 
    add_trace(x = ~x, y = ~y, z = ~z, data=basket_hoop, scene=scene, 
              type = 'scatter3d', mode = 'lines', inherit=F, showlegend=FALSE,
              line = list(width = 4, color="tomato")) %>%
    add_trace(x=basket[1], y=basket[2], z=basket[3], scene=scene,
              type = 'scatter3d', mode = 'markers', inherit=F, showlegend = FALSE,
              marker = list(size = 8, color = 'tomato')) %>%
    add_trace(x= ~x_pred, y= ~y_pred, z= ~z_pred, data=df, scene=scene,
              type = "scatter3d", mode = "lines", inherit = FALSE, showlegend = FALSE, 
              line = list(width = 8, color = est_traj_col)) %>%
    add_trace(x = ~xV, y = ~yV, z = ~zV, data=df_V, scene=scene, 
              type = "scatter3d", mode = "markers", inherit=F, showlegend = FALSE,
              marker = list(size = 8, color = vert_col)) %>%
    add_trace(x = ~c(x_pred[1],x_pred[tps]), y = ~c(y_pred[1],y_pred[tps]), 
              z = ~c(z_pred[1],z_pred[tps]), data=df, scene=scene,  
              type = "scatter3d", mode = "lines", inherit=F, showlegend = FALSE,
              line = list(width = 4, color = traj_dir_col)) %>%
    add_trace(x= ~xV_proj, y= ~yV_proj, z= ~zV_proj, data=df_V, scene=scene,
              type = "scatter3d", mode = "markers", inherit=F, showlegend = FALSE,
              marker = list(size = 8, color = vert_col)) %>%
    add_trace(x= ~c(xV,xV_proj), y= ~c(yV,yV_proj), z= ~c(zV,zV_proj), data=df_V, scene=scene, 
              type = "scatter3d", mode = "lines", inherit=F, showlegend = FALSE,
              line = list(width = 4, color = vert_col, dash="dash")) %>%
    add_trace(x = ~x, y = ~y, z = ~z, data=ideal_dir, scene=scene, 
              type = "scatter3d", mode = "lines", inherit = FALSE, showlegend = FALSE,
              line = list(width = 3, color=ideal_dir_col)) %>%
    add_trace(x= ~x, y= ~y, z= ~z, data=midpoint, scene=scene, 
              type = "scatter3d", mode = "markers", inherit=F, showlegend = FALSE,
              marker = list(size = 4, color = ideal_dir_col))
  
  if (!trace) { 
    # Add axis labels
    if (is.null(title)) {
      made_miss <- ifelse(unique(df$made_miss)==1, "MADE", "MISS")
      title <- paste0(df$id, "  ", made_miss)
    } 
    fig <- fig %>% layout(
      title = list(text = title, y = 0.1),
      scene = list(
        aspectmode = "cube",
        aspectratio = list(x = 1, y = 1, z = 1),
        xaxis = list(title = "X", range = range.x),
        yaxis = list(title = "Y", range = range.y),
        zaxis = list(title = "Z", range = NULL)
      )
    )
  }
  
  if (speed_dir) {                      
    df_tan <- data.frame(x=c(df$x_pred[1], df$x_pred[1]+coef_x[2]), 
                         y=c(df$y_pred[1], df$y_pred[1]+coef_y[2]), 
                         z=c(df$z_pred[1], df$z_pred[1]+coef_z[2]))
    fig <- fig %>%
      add_trace(x = ~x, y = ~y, z = ~z, data=df_tan, scene=scene,
                type = 'scatter3d', mode = 'lines', inherit = FALSE, showlegend = FALSE,
                line = list(width = 6, color = speed_dir_col, dash="dash"))
  }
  return(fig)
}


###################
# This function performs 
# and plots local regression 
# of log-odds
####################
plot_locreg <- function(data, yvar, xvar, breaks="Sturges", nn=0.5, np=100, xtitle="") {
  
  hist_obj <- hist(data[[xvar]], breaks=breaks, plot=FALSE)
  df_logit <- data %>%
    mutate(x_cut=cut(!!sym(xvar), breaks = hist_obj$breaks, include.lowest=T)) %>%
    group_by(x_cut, .drop=FALSE) %>%
    summarise(prop = ifelse(n()==0, 0, mean(made_miss))) %>%
    ungroup() %>%
    mutate(center=hist_obj$mids) %>%
    filter(prop!=0) %>%
    mutate(logit = log(prop / (1 - prop)))
  
  p <- ggplot(df_logit, aes(x = center, y = logit)) +
    geom_point(color="red", size=3) +
    labs(x=xtitle, y=paste0("Estimated logit of P[made | ", xvar, "]")) +
    theme_bw()

  
  frml <- as.formula(sprintf("%s ~ lp(%s, nn = %s)", yvar, xvar, nn))
  lfit <- locfit(frml, data=data, family="binomial", alpha=alpha)
  
  locfit_pred <- data %>%
    summarise(x_grid=list(seq(min(!!sym(xvar)), max(!!sym(xvar)), length.out = np))) %>%
    tidyr::unnest(x_grid) %>%
    mutate(p_pred = predict(lfit, newdata=data.frame(x_grid) %>% rename(!!xvar := x_grid),type="response"),
           logit_pred = log(p_pred / (1 - p_pred)))
  
  p <- p + geom_line(data=locfit_pred, aes(x=x_grid, y=logit_pred), 
                     colour = "blue", linewidth = 1)
  
  out_locreg=list(data=locfit_pred,plot=p)   
  return(out_locreg)
  
  }
  
###################
# This function generates data of
# a ball trajectory toward the hoop,
# given:
# starting point,
# initial speed, 
# and release angle
####################
simul_traj <- function(x0, y0, z0, vel0, ang0) {
  beta0x <- x0; beta0y <- y0; beta0z <- z0
  beta1z <- vel0 * sin(ang0*pi/180)
  beta1x <- -sqrt((vel0^2-beta1z^2)/(1+y0^2/x0^2))
  beta1y <- y0/x0*beta1x
  beta2z <- -32.174/2
  f <- function(t) beta0z + beta1z*t + beta2z*t^2-z0
  t_stop <- uniroot(f, interval = c(0.5, 10))$root
  t <- seq(0, t_stop, length.out=100)
  x <- beta0x + beta1x*t
  y <- beta0y + beta1y*t
  z <- beta0z + beta1z*t + beta2z*t^2
  df_simul <- data.frame(x, y, z)
}


##########################
# This function generates point coordinates
# for drawing white area on the RF or tree maps
##########################
whiteAreas <- function(r=34, n=200) {
  cx  <- 0
  cy  <- -41.75
  xL  <- -25
  xR  <-  25
  yTop <- 0
  y_arc <- function(x) cy + sqrt(pmax(0, r^2 - (x - cx)^2))
  yL <- y_arc(xL)
  yR <- y_arc(xR)

  x_arc <- seq(xR, xL, length.out=n)
  y_arc_vals <- y_arc(x_arc)

  df <- rbind(
    data.frame(x = xL,    y = yTop,       area="top"),
    data.frame(x = xR,    y = yTop,       area="top"),
    data.frame(x = xR,    y = yR,         area="top"),
    data.frame(x = x_arc, y = y_arc_vals, area="top"),
    data.frame(x = xL,    y = yL,         area="top"),
    data.frame(x = c(-25, 25, 3, -3),  
               y = c(-47, -47, -43, -43), area="bottom")	
  )
  return(df)
}


##################################################################
# A function for plotting spatial trees in Cartesian coordinates
##################################################################
plot_cartesian_spatial_tree <- function(fit, data, z, z.name=NULL, palette=NULL, data.team=NULL, 
                                        FUN.z=median, print.z = FALSE, area.labels=T, round.print.z=2, max.y=0,title=NULL,
                                        num_interv_legend=8, low_lim_colormap=0, up_lim_colormap=100,
										white_areas=NULL) {
  
  if (!requireNamespace("rpart", quietly = TRUE)) {
    install.packages("rpart")
  }
  if (!requireNamespace("viridis", quietly = TRUE)) {
    install.packages("viridis")
  } 
  if (!requireNamespace("PBSmapping", quietly = TRUE)) {
    install.packages("PBSmapping")
  } 
  
  
  frm <- fit$frame
  node_number <- rownames(frm)
  leaf_nodes <- as.numeric(node_number[frm$var=="<leaf>"])
  if (is.null(z.name)) z.name <- z
  
  df <- vector(length(leaf_nodes), mode="list")
  for (k in seq_along(leaf_nodes)) {
    leafk <- rpart:::path.rpart(fit,leaf_nodes[k], print.it=F)
    rulek <- c(leafk[[1]][-1],"x>-25","x<=25","y<=47.15","y>=-47.15")
    
    xmin <- rulek %>%
      grep("x>", .) %>%
      rulek[.] %>%
      strsplit(., ">") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      max()
    
    xmax <- rulek %>%
      grep("x<", .) %>%
      rulek[.] %>%
      strsplit(., "<") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      min()
    
    ymin <- rulek %>%
      grep("y>", .) %>%
      rulek[.] %>%
      strsplit(., ">") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      max()
    
    ymax <- rulek %>%
      grep("y<", .) %>%
      rulek[.] %>%
      strsplit(., "<") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      min()
    
    df[[k]] <- data.frame(xmin, xmax, ymin, ymax, node=leaf_nodes[k])
  }
  df <- do.call(rbind, df)
  
  df$z <- apply(df, 1, function(rect) {
    rect <- as.numeric(unlist(rect))
    subdata <- subset(data, x>=rect[1] & x<rect[2] & y>=rect[3] & y<rect[4])
    y <- subdata[[z]]
    FUN.z(y, na.rm=T)
  })
  df$sd.z <- apply(df, 1, function(rect) {
    rect <- as.numeric(unlist(rect))
    subdata <- subset(data, x>=rect[1] & x<rect[2] & y>=rect[3] & y<rect[4])
    y <- subdata[[z]]
    sd(y, na.rm=T)
  })
  df$n <- apply(df, 1, function(rect) {
    rect <- as.numeric(unlist(rect))
    subdata <- subset(data, x>=rect[1] & x<rect[2] & y>=rect[3] & y<rect[4])
    y <- subdata[[z]]
    sum(!is.na(y))
  })
  
  
  if (!is.null(data.team)) {
    df$zteam <- apply(df, 1, function(rect) {
      rect <- as.numeric(unlist(rect))
      subdata <- subset(data.team, x>=rect[1] & x<=rect[2] & y>=rect[3] & y<rect[4])
      y <- subdata[[z]]
      FUN.z(y, na.rm=T)
    })
    df$z <- df$z - df$zteam
    lowlim_clrmap <- -max(abs(df$z))
    uplim_clrmap  <- max(abs(df$z))
  } else {
    lowlim_clrmap <- min(df$z)
    uplim_clrmap  <- max(df$z)
  }
  if (low_lim_colormap<lowlim_clrmap) lowlim_clrmap <- low_lim_colormap
  if (up_lim_colormap>uplim_clrmap) uplim_clrmap <- up_lim_colormap
  lowlim_clrmap <- floor(lowlim_clrmap*10)/10
  uplim_clrmap <- ceiling(uplim_clrmap*10)/10
  
  stats_by_sect <- sapply(df$node, function(k) {
    sectk <- subset(df, node==k)
    filtk <- sp::point.in.polygon(point.x=data$x, point.y=data$y, 
                                  pol.x=sectk[,c("xmin", "xmax", "xmax", "xmin")], 
                                  pol.y=sectk[,c("ymin", "ymin", "ymax", "ymax")])==1
    mnk <- round(FUN.z(data[[z]][filtk], na.rm=T),round.print.z)
    if (!is.null(data.team)) {
      filtk.team <- sp::point.in.polygon(point.x=data.team$x, point.y=data.team$y, 
                                         pol.x=sectk[,c("xmin", "xmax", "xmax", "xmin")], 
                                         pol.y=sectk[,c("ymin", "ymin", "ymax", "ymax")])==1
      mnk.team <- round(FUN.z(data.team[[z]][filtk.team], na.rm=T),round.print.z)	
      mnk <- mnk - mnk.team
    }
    totk <- sum(filtk)
    madek <- sum(data$result[filtk]=="made")
    pctk <- round(100*madek/totk)
    c(mnk,madek,totk,pctk,k)
  })
  
  df$ymax <- sapply(df$ymax, function(x) min(x,0))      
  sects <- data.frame(X=c(df$xmin,df$xmax,df$xmax,df$xmin), 
                      Y=c(df$ymin,df$ymin,df$ymax,df$ymax),
                      POS=rep(1:4, each=nrow(df)),
                      PID=rep(1:nrow(df), 4))
  idx <- order(sects$PID)
  sects <- sects[idx,]
  
  s <- PBSmapping::as.PolySet(sects)
  if (print.z) {
    centroids <- data.frame(PBSmapping::calcCentroid(s),
                            text=paste0(stats_by_sect[1,],"\n", stats_by_sect[4,],"% (",stats_by_sect[2,],"/",stats_by_sect[3,],")"))
  } else {
    centroids <- data.frame(PBSmapping::calcCentroid(s),
                            text=paste0(stats_by_sect[4,],"% (",stats_by_sect[2,],"/",stats_by_sect[3,],")"))  
  }
  
  #################
  # Plot colored rectangles and court lines
  #################
  df$z_cat <- cut(df$z, include.lowest=TRUE, na.rm=TRUE,
                  breaks=round(seq(lowlim_clrmap,uplim_clrmap,length.out=num_interv_legend+1),1) )    
  
  # Define color palette				  
  if (is.null(palette)) {
    pal <- function(n) {
      viridis::viridis(n, option = "viridis", direction = 1)
    }
  } else {
    pal <- BasketballAnalyzeR:::BbA_pal(palette=palette)
  }
  n_levels <- length(levels(df$z_cat))
  palette_colors <- pal(n_levels)
  names(palette_colors) <- levels(df$z_cat)	

  wAreas <- whiteAreas() %>% filter(area %in% white_areas)
  
  p <- ggplot() + 
    geom_rect(data=df, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z_cat), show.legend = TRUE) +
    geom_polygon(data = wAreas, aes(x=x, y=y, group=area),
                 fill = "white", color = NA, inherit.aes = FALSE) +		
    scale_fill_manual(name=z.name, values=palette_colors, drop=FALSE) +
    coord_fixed() + labs(caption=title) + theme_void()
  p <- drawNBAcourt(p) 
  if (area.labels) {
    p <- p + geom_text(data=centroids, aes(x=X,y=Y, label=text), col="white")
  }
  return(list(rect=df, plotTree=p, stats_by_sect=stats_by_sect, centroids=centroids))
}


############################################################
# Converts Cartesian coordinates to polar coordinates and 
# returns a data frame with r (radius) and theta (angle). 
# Angles below –π/2 are adjusted to the [0, 2π) range
############################################################
cart2polar <- function(x, y) {
  theta <- atan2(y, x)
  theta <- ifelse(theta < -pi/2, 2*pi + theta, theta)
  data.frame(r = sqrt(x^2 + y^2), theta = theta)
}


############################################################
# Converts polar coordinates to Cartesian coordinates and 
# returns a data frame with x and y values
############################################################
polar2cart <- function(r, theta) {
  data.frame(x = r * cos(theta), y = r * sin(theta))
}


##################################################################
# A function for plotting spatial trees in polar coordinates
##################################################################
plot_polar_spatial_tree <- function(fit, data, z, z.name=NULL, palette="mixed", data.team=NULL, FUN.z=median, 
                                    print.z = FALSE, area.labels=T, round.print.z=2, max.y=0, xc=0, yc=-41.75, npt=100, col_labs="white",
                                    size_labs=3, title=NULL, expand_plot=FALSE, 
                                    num_interv_legend=8, low_lim_colormap=0, up_lim_colormap=100,
									white_areas=NULL) {
  
  frm <- fit$frame
  node_number <- rownames(frm)
  leaf_nodes <- as.numeric(node_number[frm$var=="<leaf>"])
  if (is.null(z.name)) z.name <- z
  
  df <- vector(length(leaf_nodes), mode="list")
  for (k in seq_along(leaf_nodes)) {
    leafk <- rpart:::path.rpart(fit,leaf_nodes[k], print.it=F)
    #rulek <- c(leafk[[1]][-1],"rho>=0","rho<=48.66274","theta<=3.348585","theta>=-0.2069922")
    rulek <- c(leafk[[1]][-1],"rho>=0","rho<=92.20392","theta<=4.712389","theta>=-1.570796")
    rho_min <- rulek %>%
      grep("rho>", .) %>%
      rulek[.] %>%
      strsplit(., ">") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      max()
    
    rho_max <- rulek %>%
      grep("rho<", .) %>%
      rulek[.] %>%
      strsplit(., "<") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      min()
    
    theta_min <- rulek %>%
      grep("theta>", .) %>%
      rulek[.] %>%
      strsplit(., ">") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      max()
    
    theta_max <- rulek %>%
      grep("theta<", .) %>%
      rulek[.] %>%
      strsplit(., "<") %>%
      lapply(., "[[",2) %>%
      unlist() %>%
      gsub("=","",.) %>%
      as.numeric() %>%
      min()
    
    df[[k]] <- data.frame(rho_min, rho_max, theta_min, theta_max, node=leaf_nodes[k])
  }
  df <- do.call(rbind, df)
  
  df$z <- apply(df, 1, function(rect) {
    rect <- as.numeric(unlist(rect))
    subdata <- subset(data, rho>=rect[1] & rho<rect[2] & theta>=rect[3] & theta<rect[4])
    y <- subdata[[z]]
    FUN.z(y, na.rm=T)
  }) 
  df$sd.z <- apply(df, 1, function(rect) {
    rect <- as.numeric(unlist(rect))
    subdata <- subset(data, rho>=rect[1] & rho<rect[2] & theta>=rect[3] & theta<rect[4])
    y <- subdata[[z]]
    sd(y, na.rm=T)
  })
  df$n <- apply(df, 1, function(rect) {
    rect <- as.numeric(unlist(rect))
    subdata <- subset(data, rho>=rect[1] & rho<rect[2] & theta>=rect[3] & theta<rect[4])
    y <- subdata[[z]]
    sum(!is.na(y))
  })
  
  if (!is.null(data.team)) {
    df$zteam <- apply(df, 1, function(rect) {
      rect <- as.numeric(unlist(rect))
      subdata <- subset(data.team, rho>=rect[1] & rho<=rect[2] & theta>=rect[3] & theta<rect[4])
      y <- subdata[[z]]
      FUN.z(y, na.rm=T)
    })
    df$z <- df$z - df$zteam
    lowlim_clrmap <- -max(abs(df$z))
    uplim_clrmap  <- max(abs(df$z))
  } else {
    lowlim_clrmap <- min(df$z)
    uplim_clrmap  <- max(df$z)
  }
  if (low_lim_colormap<lowlim_clrmap) lowlim_clrmap <- low_lim_colormap
  if (up_lim_colormap>uplim_clrmap) uplim_clrmap <- up_lim_colormap
  lowlim_clrmap <- floor(lowlim_clrmap*10)/10
  uplim_clrmap <- ceiling(uplim_clrmap*10)/10
  
  ### Statistics for z inside the sectors
  stats_by_sect <- sapply(df$node, function(k) {
    sectk <- subset(df, node==k)
    filtk <- sp::point.in.polygon(point.x=data$rho, point.y=data$theta, 
                                  pol.x=sectk[,c("rho_min", "rho_max", "rho_max", "rho_min")], 
                                  pol.y=sectk[,c("theta_min", "theta_min", "theta_max", "theta_max")])==1
    mnk <- round(FUN.z(data[[z]][filtk], na.rm=T),round.print.z)
    if (!is.null(data.team)) {
      filtk.team <- sp::point.in.polygon(point.x=data.team$rho, point.y=data.team$theta, 
                                         pol.x=sectk[,c("rho_min", "rho_max", "rho_max", "rho_min")], 
                                         pol.y=sectk[,c("theta_min", "theta_min", "theta_max", "theta_max")])==1
      mnk.team <- round(FUN.z(data.team[[z]][filtk.team], na.rm=T),round.print.z)	
      mnk <- mnk - mnk.team
    }
    totk <- sum(filtk)
    madek <- sum(data$result[filtk]=="made")
    pctk <- round(100*madek/totk)
    c(mnk,madek,totk,pctk,k)
  })
  stats_by_sect <- format(stats_by_sect, ndigits=2)  
  
  ### Build rectangles (i.e. sectors) in cartesian coordinates
  df_rect <- vector(nrow(df), mode="list")
  df_rect <- lapply(1:nrow(df), function(k) {
    nodek <- df$node[k]
    sectk <- subset(df, node==nodek)
    dfk <- with(sectk, rect_pol(theta_min,rho_min,theta_max,rho_max, xc, yc, npt))
    dfk <- rbind(dfk, dfk[1,])
    dfk$z <- sectk$z
    dfk$POS <- 1:nrow(dfk)
    dfk$PID <- k
    return(dfk)
  })
  df_rect <- do.call(rbind, df_rect)
  idx <- order(df_rect$PID, df_rect$POS)
  df_rect <- df_rect[idx,]  
  
  ###  Label positions
  lab_pos <- t(apply(df, 1, function(sectk) {
    rho_centr <- (sectk[1]+sectk[2])/2
    theta_centr <- (sectk[3]+sectk[4])/2
    if (theta_centr>pi & sectk[4]>3.348585) { theta_centr <- (sectk[3]+3.348585)/2 }
    if (theta_centr<0 & sectk[3]< -0.2069922) { theta_centr <- (-0.2069922+sectk[4])/2	}
    xy <- polar2cart(r=rho_centr, theta=theta_centr)	
    if (abs(xy$x)>24) { rho_centr <- sqrt(24^2+xy$y^2) }
    if (abs(xy$y)>41.25) { rho_centr <- sqrt(xy$x^2+ ((sectk[1]+41.25)/2)^2) }
    xy <- polar2cart(r=rho_centr, theta=theta_centr)
    c(rho_centr, theta_centr, x=xy$x+xc, y=xy$y+yc)
  }))
  
  if (print.z) {
    lab_pos <- data.frame(lab_pos,
                          text=paste0(stats_by_sect[1,], stats_by_sect[4,],"% (",stats_by_sect[2,],"/",stats_by_sect[3,],")"))
  } else {
    lab_pos <- data.frame(lab_pos,
                          text=paste0(stats_by_sect[4,],"% (",stats_by_sect[2,],"/",stats_by_sect[3,],")"))  
  }
  names(lab_pos) <- c("rho","theta","x","y","text")
  
  ### Plot colored rectangles (sectors) and court lines
  df_rect$z_cat <- cut(df_rect$z, include.lowest=TRUE, na.rm=TRUE,
                       breaks=round(seq(lowlim_clrmap,uplim_clrmap,length.out=num_interv_legend+1),1) )
  pal <- BasketballAnalyzeR:::BbA_pal(palette=palette)
  n_levels <- length(levels(df_rect$z_cat))
  palette_colors <- pal(n_levels)
  names(palette_colors) <- levels(df_rect$z_cat)
  
  wAreas <- whiteAreas() %>% filter(area %in% white_areas)
  
  p <- ggplot() + 
    geom_polygon(data=df_rect, mapping=aes(x=x, y=y, fill=z_cat, group=PID), color="gray50", show.legend=T) +
    geom_polygon(data = wAreas, aes(x=x, y=y, group=area),
                 fill = "white", color = NA, inherit.aes = FALSE) +		
    scale_fill_manual(name=z.name, values=palette_colors, drop=FALSE) +
    labs(caption=title) +
    theme_void()
  if  (!expand_plot) p <- p + coord_fixed(xlim=c(-25,25), ylim=c(-47,0), expand=F)
  p <- drawNBAcourt(p) 
  if (area.labels) {
    p <- p + geom_text(data=lab_pos, aes(x=x,y=y, label=text, angle=180+(90+theta*180/pi)), col=col_labs, size=size_labs, inherit.aes=F)
  }
  return(list(rect=df, plotTree=p, stats_by_sect=stats_by_sect, lab_pos=lab_pos, coord_rect=df_rect))
}


############################################################
# Generates the coordinates of a closed rectangular path defined in polar coordinates. 
# The rectangle is specified by angular bounds (theta_left, theta_right), 
# radial bounds (rho_bottom, rho_top), and a center point (xc, yc). 
# The parameter n controls the resolution (number of points) per side. 
# Returns a data frame with x and y values in Cartesian coordinates
############################################################
rect_pol <- function(theta_left, rho_bottom, theta_right, rho_top, xc, yc, n) {
  # Side 1
  theta_seq <- seq(theta_left, theta_right, length.out=n)
  rho_seq <- rep(rho_bottom, n)
  x_seq <- rho_seq*cos(theta_seq)+xc
  y_seq <- rho_seq*sin(theta_seq)+yc
  df <- data.frame(x=x_seq, y=y_seq)
  # Side 2
  theta_seq <- rep(theta_right, n)
  rho_seq <- seq(rho_bottom, rho_top, length.out=n)
  x_seq <- rho_seq*cos(theta_seq)+xc
  y_seq <- rho_seq*sin(theta_seq)+yc
  df <- rbind(df, data.frame(x=x_seq, y=y_seq))
  # Side 3
  theta_seq <- seq(theta_right, theta_left, length.out=n)
  rho_seq <- rep(rho_top, n)
  x_seq <- rho_seq*cos(theta_seq)+xc
  y_seq <- rho_seq*sin(theta_seq)+yc
  df <- rbind(df, data.frame(x=x_seq, y=y_seq))
  # Side 4
  theta_seq <- rep(theta_left, n)
  rho_seq <- seq(rho_top, rho_bottom, length.out=n)
  x_seq <- rho_seq*cos(theta_seq)+xc
  y_seq <- rho_seq*sin(theta_seq)+yc
  df <- rbind(df, data.frame(x=x_seq, y=y_seq))
}


####################################################################################
# Plots the ROC curve and the AUC starting from randomForest and ranger (Extratrees)
####################################################################################
plot_RF_ROC <- function(RF, data) {
  
  pred_RF <- if (inherits(RF, "randomForest")) {
    100*predict(RF, newdata=data, type="prob")[, "made"]
  } else if (inherits(RF, "ranger")) {
    100*rowMeans(predict(RF, data=data, predict.all=T)$predictions-1)
  } else if (inherits(RF, "ObliqueForestClassification")) {
    100*predict(object=RF, new_data=data, pred_type="prob", oobag = F)[, 2]	
  } else {
    NA_real_
  }
  
  df_roc <- data.frame(
    D = ifelse(data$result=="made", 1, 0),  
    M = pred_RF
  )
  
  ROCcurve <- roc(df_roc$D, df_roc$M)
  AUC_CI <- round(ci.auc(ROCcurve, method="delong"),2)
 
 auc_label = if (inherits(RF, "randomForest")) {
 				sprintf("Random Forest - ROC curve \nAUC=%s (95%% CI %s - %s)", 
                AUC_CI[2], AUC_CI[1], AUC_CI[3])
  } else if (inherits(RF, "ranger")) {
     			sprintf("Extra trees - ROC curve \nAUC=%s (95%% CI %s - %s)", 
                AUC_CI[2], AUC_CI[1], AUC_CI[3])
  } else if (inherits(RF, "ObliqueForestClassification")) {
     			sprintf("Oblique Random Forest - ROC curve \nAUC=%s (95%% CI %s - %s)", 
                AUC_CI[2], AUC_CI[1], AUC_CI[3])
  }
 
   
  p_rocRF <- ggplot() + 
    geom_roc(data=df_roc, aes(d=D, m=M), n.cuts=0) +                        
    geom_segment(aes(x=0, y=0, xend=1, yend=1), color="black", 
                 linetype="solid", linewidth=0.5) + 
    annotation_custom(
      grob = textGrob(auc_label,
                      x = unit(0.02, "npc"),
                      y = unit(0.98, "npc"),
                      just = c("left", "top")) ) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    labs(x="False Positive Rate (1 - Specificity)",
         y="True Positive Rate (Sensitivity)")  + 
    theme_bw()
  
  invisible(p_rocRF)
}


####################################################################################
# Creates scoring probability maps starting from randomForest and ranger (Extratrees)
####################################################################################
plot_RF_map <- function(RF, n_grid=150, Xc=0, Yc=-41.75, colormap_bins=0, white_areas=c("top","bottom")) {
  
  polar_grid <- expand.grid(x=seq(-25,25,length.out=n_grid), y=seq(-47,0,length.out=n_grid)) %>%
    mutate(cart2polar(x-Xc, y-Yc)) %>%
    rename(rho=r) %>%
    mutate(
      prob_RF = if (inherits(RF, "randomForest")) {
        100*predict(RF, newdata=., type="prob")[, "made"]
      } else if (inherits(RF, "ranger")) {
        100*rowMeans(predict(RF, data=., predict.all=T)$predictions-1)
      } else if (inherits(RF, "ObliqueForestClassification")) {
        100*predict(object=RF, new_data=., pred_type="prob", oobag = F)[, 2]			
      } else {
        NA_real_
      })
	  
  wAreas <- whiteAreas() %>% filter(area %in% white_areas)
	  
  p_mapRF <- ggplot(data=polar_grid) +
    #geom_contour_filled(aes(x=x, y=y, z=prob_RF), bins=8, show.legend=TRUE) +
	geom_raster(aes(x = x, y = y, fill = prob_RF), show.legend=TRUE) +
    geom_polygon(data = wAreas, aes(x=x, y=y, group=area),
                 fill = "white", color = NA, inherit.aes = FALSE) +	
    coord_fixed() +
    theme_void()

  if (colormap_bins==0) {	
    p_mapRF <- p_mapRF + scale_fill_viridis_c(name="Scoring\nprobability %", 
	                                          limits = c(0, 100))
  } else { 
	p_mapRF <- p_mapRF + scale_fill_viridis_b(limits = c(0, 100),
                         breaks = seq(0, 100, length.out = colormap_bins+1),
                         direction = 1, name="Scoring\nprobability %")
   }

  p_mapRF <- drawNBAcourt(p_mapRF)
  
  invisible(p_mapRF)
}



