#' @title FIS evaluation
#' @author Jon Garibaldi, Chao Chen, Tajul Razak
#' @description Evaluate a Fuzzy Inference System (fis). For more details, please see FuzzyR package.
#' @param input_stack A matrix representing the input stack, number of inputs (columns) by number of outputs (rows).
#' @param fis A fis must be provided.
#' @return  An evaluated crisp value for a given fis structure. 

  evalfis <- function(input_stack, fis) {

  point_n= 101
  if ( !exists("GLOBAL_FIS") || !identical(fis, .GlobalEnv$GLOBAL_FIS) ) {

    # Add '.GlobalEnv$' to replace super assignment '<<-' (by Chao & Tajul)

    #print("initialising ...")
    .GlobalEnv$GLOBAL_FIS    <- fis
    .GlobalEnv$FIS_TYPE      <- fis$type
    .GlobalEnv$IN_N          <- length(fis$input)
    .GlobalEnv$OUT_N         <- length(fis$output)
    .GlobalEnv$IN_MF_N       <- NULL
    .GlobalEnv$OUT_MF_N      <- NULL
    for ( i in 1:.GlobalEnv$IN_N )
      .GlobalEnv$IN_MF_N[i]  <- length(fis$input[[i]]$mf)
    for ( i in 1:.GlobalEnv$OUT_N )
      .GlobalEnv$OUT_MF_N[i] <- length(fis$output[[i]]$mf)

    .GlobalEnv$RULE_N        <- nrow(fis$rule)
    .GlobalEnv$RULE_LIST     <- fis$rule[,1:(.GlobalEnv$IN_N+.GlobalEnv$OUT_N)]
    .GlobalEnv$RULE_ANTE     <- fis$rule[,1:.GlobalEnv$IN_N]
    .GlobalEnv$RULE_CONS     <- fis$rule[,(.GlobalEnv$IN_N+1):(.GlobalEnv$IN_N+.GlobalEnv$OUT_N)]
    .GlobalEnv$RULE_WEIGHT   <- fis$rule[,.GlobalEnv$IN_N+.GlobalEnv$OUT_N+1]
    .GlobalEnv$AND_OR        <- fis$rule[,.GlobalEnv$IN_N+.GlobalEnv$OUT_N+2]
    .GlobalEnv$AND_METHOD    <- fis$andMethod
    .GlobalEnv$OR_METHOD     <- fis$orMethod
    .GlobalEnv$IMP_METHOD    <- fis$impMethod
    .GlobalEnv$AGG_METHOD    <- fis$aggMethod
    .GlobalEnv$DEFUZZ_METHOD <- fis$defuzzMethod

    # get input params and types into globals
    .GlobalEnv$IN_TYPE <- NULL
    .GlobalEnv$IN_PARAMS <- list()

    idx= 1
    for ( i in 1:.GlobalEnv$IN_N ) {
      for ( j in 1:.GlobalEnv$IN_MF_N[i] ) {
        .GlobalEnv$IN_TYPE[idx] <- fis$input[[i]]$mf[[j]]$type
        .GlobalEnv$IN_PARAMS[idx] <- list(fis$input[[i]]$mf[[j]]$params)
        idx= idx + 1
      }
    }

    # compute OUT_TEMPLATE_MF: matrix (OUT_MF_N_TOTAL+1 X point_n)
    .GlobalEnv$OUT_RANGE <- matrix(0, .GlobalEnv$OUT_N, 2)
    .GlobalEnv$OUT_TEMPLATE_MF <- matrix(0, sum(.GlobalEnv$OUT_MF_N)+1, point_n)

    # dont care MF
    .GlobalEnv$OUT_TEMPLATE_MF[1,] <- 1
    idx= 1
    for ( i in 1:.GlobalEnv$OUT_N ) {
      for ( j in 1:.GlobalEnv$OUT_MF_N[i] ) {
        .GlobalEnv$OUT_RANGE[i,] <- fis$output[[i]]$range
        .GlobalEnv$OUT_TEMPLATE_MF[idx+1,] <-
          evalmf(seq(.GlobalEnv$OUT_RANGE[i,1], .GlobalEnv$OUT_RANGE[i,2], length=point_n),
            fis$output[[i]]$mf[[j]]$type, fis$output[[i]]$mf[[j]]$params)
        idx= idx + 1
      }
    }

    # reorder to fill OUT_MF, an (RULE_N X point_n*OUT_N) matrix
    # idx= abs(.GlobalEnv$RULE_CONS)+matrix((0:(.GlobalEnv$OUT_N-1))*.GlobalEnv$RULE_N+1, .GlobalEnv$RULE_N, .GlobalEnv$OUT_N, byrow=TRUE)
    ## modified by Chao, 20 April 2017
    idx= abs(.GlobalEnv$RULE_CONS)+matrix(c(0, cumsum(.GlobalEnv$OUT_MF_N)[-.GlobalEnv$OUT_N]) + 1, .GlobalEnv$RULE_N, .GlobalEnv$OUT_N, byrow=TRUE)

    idx[.GlobalEnv$RULE_CONS==0]= 1
    .GlobalEnv$OUT_MF <- .GlobalEnv$OUT_TEMPLATE_MF[t(idx),]
    .GlobalEnv$OUT_MF[t(.GlobalEnv$RULE_CONS)<0,] <- 1 - .GlobalEnv$OUT_MF[t(.GlobalEnv$RULE_CONS<0)]
    .GlobalEnv$OUT_MF <- matrix(t(.GlobalEnv$OUT_MF), .GlobalEnv$RULE_N, point_n*.GlobalEnv$OUT_N, byrow=TRUE)

    # allocate other matrices
    .GlobalEnv$QUALIFIED_OUT_MF <- matrix(0, .GlobalEnv$RULE_N, point_n*.GlobalEnv$OUT_N)
    .GlobalEnv$OVERALL_OUT_MF <- matrix(0, point_n*.GlobalEnv$OUT_N)
  }
  # end of initialisation

  # error checking for input stack
  if ( is.vector(input_stack) ) {
    input_stack= rbind(input_stack)
  }
  data_n= nrow(input_stack)

  # allocate output stack
  output_stack= matrix(0, data_n, .GlobalEnv$OUT_N)

  # iteration through each row of input stack
  for ( kkk in 1:data_n ) {
    input= input_stack[kkk,]

    # get in_template_mf_value, a value for each MF
    in_template_mf_value= rep(0, sum(.GlobalEnv$IN_MF_N))
    idx= 1
    for ( i in 1:.GlobalEnv$IN_N ) {
      for ( j in 1:.GlobalEnv$IN_MF_N[i] ) {
        in_template_mf_value[idx]=
          evalmf(input[i], .GlobalEnv$IN_TYPE[idx], .GlobalEnv$IN_PARAMS[[idx]])
        idx= idx + 1
      }
    }
    # add a leading zero: (fixes problem with missing rules clauses in first rule)
    in_template_mf_value= c(0, in_template_mf_value)

    # reordering to get in_mf_value, a (RULE_N X .GlobalEnv$IN_N) matrix
    index= matrix(1, .GlobalEnv$RULE_N, 1) %*% cumsum(c(0, .GlobalEnv$IN_MF_N[1:(.GlobalEnv$IN_N-1)])) + abs(.GlobalEnv$RULE_ANTE) + 1
    in_mf_value= matrix(in_template_mf_value[index], .GlobalEnv$RULE_N, .GlobalEnv$IN_N)

    # replace dont-care MFs in AND rules with 1, and OR rules with 0
    in_mf_value[which(((.GlobalEnv$AND_OR == 1) * (.GlobalEnv$RULE_ANTE == 0)) == 1)]= 1
    in_mf_value[which(((.GlobalEnv$AND_OR == 2) * (.GlobalEnv$RULE_ANTE == 0)) == 1)]= 0

    # take care of NOTs (negative rule indices)
    idx= which(.GlobalEnv$RULE_ANTE < 0)
    in_mf_value[idx]= 1 - in_mf_value[idx]

    #cat(in_mf_value, '\n')

    # find firing strengths of rules
    firing_strength= matrix(0, .GlobalEnv$RULE_N, 1)
    if ( .GlobalEnv$IN_N == 1 )
      firing_strength= in_mf_value
    else {
      and_index= which(.GlobalEnv$AND_OR == 1)
      or_index= which(.GlobalEnv$AND_OR == 2)
      firing_strength[and_index]=
        apply(rbind(in_mf_value[and_index,]), 1, .GlobalEnv$AND_METHOD)
      firing_strength[or_index]=
        apply(rbind(in_mf_value[or_index,]), 1, .GlobalEnv$OR_METHOD)
    }

    firing_strength= firing_strength * .GlobalEnv$RULE_WEIGHT
    # cat(firing_strength, '\n')

    if ( .GlobalEnv$FIS_TYPE == 'mamdani' )
    {
      # transform OUT_MF to OUT_QUALIFIED_MF
      tmp= matrix(firing_strength, nrow(firing_strength), point_n*.GlobalEnv$OUT_N)
      if ( .GlobalEnv$IMP_METHOD == 'prod' ) {
        .GlobalEnv$QUALIFIED_OUT_MF <- tmp * .GlobalEnv$OUT_MF
      } else if ( .GlobalEnv$IMP_METHOD == 'min' ) {
        .GlobalEnv$QUALIFIED_OUT_MF <- pmin(tmp, .GlobalEnv$OUT_MF)
      } else {
        cat('user-defined implication not implemented yet\n')
      }

      # aggregation: 'sum', 'max', 'probor' or user-defined
      .GlobalEnv$OVERALL_OUT_MF <- apply(.GlobalEnv$QUALIFIED_OUT_MF, 2, .GlobalEnv$AGG_METHOD)

      # defuzzify each output
      for ( i in 1:.GlobalEnv$OUT_N ) {
        output_stack[kkk, i]=
          defuzz(seq(.GlobalEnv$OUT_RANGE[i, 1], .GlobalEnv$OUT_RANGE[i, 2], length=point_n),
            .GlobalEnv$OVERALL_OUT_MF[((i-1)*point_n+1):(i*point_n)], .GlobalEnv$DEFUZZ_METHOD)
        # add by Tajul
        .GlobalEnv$D_x <- seq(.GlobalEnv$OUT_RANGE[i, 1], .GlobalEnv$OUT_RANGE[i, 2], length=point_n)
        .GlobalEnv$D_y <- .GlobalEnv$OVERALL_OUT_MF[((i-1)*point_n+1):(i*point_n)]
        # end
      }
    }
    else if ( .GlobalEnv$FIS_TYPE == 'sugeno' )
    {
      cat('sugeno inference not implemented yet\n')
    }
    else {
      cat('unknown inference type\n')
    }
  }
  # add by Tajul
  .GlobalEnv$D_out <- round(output_stack,2)
  #end
  output_stack
}
