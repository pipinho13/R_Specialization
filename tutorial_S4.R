####S4 Tutorial
setClass("BMI", representation(weight="numeric", size="numeric"))

setMethod("show", "BMI",
           function(object){cat("BMI=",object@weight/(object@size^2)," \n ")}
          )


### Creation of an object for me, and posting of my BMI
myBMI <- new("BMI",weight=85,size=1.84)
#BMI= 25.10633

### Creation of an object for her, and posting of her BMI
herBMI <- new("BMI",weight=62,size=1.60)
#BMI= 24.21875

#### Object programming, control
 setValidity("BMI",
                 function(object){if(object@size<0){return("negative Size")}else{return(TRUE)}}
                 )
 
 new("BMI",weight=85,size=-1.84)
# Error in validObject(.Object) : invalid class “BMI” object: negative Size
 
 ### Inheritance:
 ### Definition of the heir
 setClass("BMIplus",representation(sex="character"),contains="BMI")
 
 he <- new("BMIplus",size=1.76,weight=84,sex="Male")
 he
 # BMI= 27.11777 
 
 # Encapsulation:finally, object programming enables to define all the tools concerning
 # an object and to lock them up in blocks, without having to look after them anymore.
 # That is called encapsulation. Cars provide a good example of encapsulation: once the
 # hood closed, one does not need to know anymore about the details of the mechanics
 # to drive. Similarly, once the object finished and closed, a user does not have to worry
 # about its working procedure. Even better, concerning the cars, it is not possible to be
 # mistaken and to put gasoline in the radiator since the radiator is not accessible anymore.
 # In the same way, encapsulation enables to protect what must be protected, and to give
 # access only to what is not at risk.
 
 
 
 setClass(
    Class="Trajectories",
    representation=representation(
      times = "numeric",
     traj = "matrix"
     )
    )
 
 
 #When a class exists, we can create an object of its class using the constructor new:
 new(Class="Trajectories")
 
 new(Class="Trajectories",times=c(1,3,4))
 
 new(Class="Trajectories",times=c(1,3),traj=matrix(1:4,ncol=2))
 
 
 # An object can be stored in a variable like any other value of R. To illustrate our
 # statements, we are going to build up a small example. Three hospitals take part to the
 # study. The Piti´e Salpˆetriere (which has not yet returned its data file, shame on them!),
 # Cochin and Saint-Anne:
 trajPitie <- new(Class="Trajectories")
 
 
 trajCochin <- new(
    Class= "Trajectories",
   times=c(1,3,4,5),
   traj=rbind (
    c(15,15.1, 15.2, 15.2),
     c(16,15.9, 16,16.4),
    c(15.2, NA, 15.3, 15.3),
     c(15.7, 15.6, 15.8, 16)
     )
   )
 
 
 
 trajStAnne <- new(
   Class= "Trajectories",
   times=c(1: 10, (6: 16) *2),
   traj=rbind(
    matrix (seq (16,19, length=21), ncol=21, nrow=50, byrow=TRUE),
    matrix (seq (15.8, 18, length=21), ncol=21, nrow=30, byrow=TRUE)
    )rnorm (21*80,0,0.2)
    )
 
 
 
 
### 4.4 Default values
 # One can declare an object by giving it default values. With each creation, if the user
 # does not specify the values of slots, they will have one nevertheless. Therefore, one must
 # add the prototype argument to the definition of the object:
    setClass(
      Class = "TrajectoriesBis",
      representation=representation(
        time = "numeric",
        traj = "matrix"
        ),
      prototype=prototype(
        time = 1,
        traj = matrix (0)
        )
      )
 
 # 
 # 4.5 To remove an object
 # In the particular case of the object trajectories, there is no real default value which
 # is essential. It is thus preferable to preserve class as it was initially defined. The class
 # TrajectoriesBis does not have any utility. One can remove it using removeClass:
    removeClass("TrajectoriesBis")
 #[1] TRUE
  new(Class="TrajectoiresBis")
  # Error in getClass(Class, where = topenv(parent.frame())) :
  #   "TrajectoiresBis" is not a
  # defined class
  
  
 # To reach a slot
  trajCochin@times
 # [1] 1 3 4 5  
  
  trajCochin@times <- c(1,2,4,5)
  
  trajCochin
  # An object of class "Trajectories"
  # Slot "times":
  #   [1] 1 3 4 5
  # 
  # Slot "traj":
  #   [,1] [,2] [,3] [,4]
  # [1,] 15.0 15.1 15.2 15.2
  # [2,] 16.0 15.9 16.0 16.4
  # [3,] 15.2   NA 15.3 15.3
  # [4,] 15.7 15.6 15.8 16.0
  
  # As we will see thereafter, the use of the @ should be avoided. Indeed, it does not
  # call upon the methods of checking. The use that we present here (posting of a field, and
  # even worse, assignment of a value to a field) should thus be proscribed in most cases.
  
  
  ###Default Values
  
  # One can declare an object by giving it default values. With each creation, if the user
  # does not specify the values of slots, they will have one nevertheless. Therefore, one must
  # add the prototype argument to the definition of the object:
     setClass(
       Class = "TrajectoriesBis",
       representation=representation(
         time = "numeric",
         traj = "matrix"
         ),
       prototype=prototype(
         time = 1,
         traj = matrix (0)
         )
       )
  # [1] "TrajectoriesBis"
  
  # Removing the definition of a class does not remove the methods which
  # are associated to it. To really remove a class, it is necessary to remove the
  # class then to remove all its methods...
  
  
  ##################
  ###To see an object
  ###################
  
  # slotNames gives the name of the slots as a vector of type character.
  # getSlots gives the name of the slots and their type. 
  # getClass gives the names of slots and their type, but also heirs and ancestors. Asthe heritage is still “terra incognita” for the moment, 
  # it doesn’t make any difference.
  
  
 slotNames("Trajectories")
 # [1] "times" "traj"
  
  getSlots ("Trajectories")
  #times traj
  #"numeric" "matrix"
  
  getClass ("Trajectories")
  #Slots:
  #  Name: times traj
  # Class: numeric matrix
  
  
  
#Methods
  
   setMethod(
     f= "plot",
     signature= "Trajectories",
     definition=function (x,y,...){
       matplot(x@times,t(x@traj),xaxt="n",type="l",ylab= "",xlab="", pch=1)
       axis(1,at=x@times)
       }
     )
   par(mfrow=c (1,2))
   plot(trajCochin)
   plot(trajStAnne)
   
   args(plot)
   # function (x, y, ...)
   #   NULL
   
   
   
   ###Show and Print 
   
   # In the same way, we define show and print for the trajectories. args(print) indicates
   # that print takes for argument (x,...). Thus:
   
   
      setMethod ("print","Trajectories",
                   function(x,...){
                     cat("*** Class Trajectories, method Print *** \n")
                     cat("* Times ="); print (x@times)
                     cat("* Traj = \n"); print (x@traj)
                     cat("******* End Print (trajectories) ******* \n")
                     }
                   )
   
   
   print(trajCochin)
   
   # *** Class Trajectories, method Print *** 
   #   * Times =[1] 1 2 4 5
   # * Traj = 
   #   [,1] [,2] [,3] [,4]
   # [1,] 15.0 15.1 15.2 15.2
   # [2,] 16.0 15.9 16.0 16.4
   # [3,] 15.2   NA 15.3 15.3
   # [4,] 15.7 15.6 15.8 16.0
   # ******* End Print (trajectories) ******* 
   
   
   
   
   # For Cochin, the result is correct. For Saint-Anne, print will display too much
   # information. So we need a second method.
   # show is the default method used to show an object when its name is write in the
   # console. We thus define it by taking into account the size of the object: if there are too
   # many trajectories, show post only part of them.
   
   setMethod("show","Trajectories",
                function(object){
                  cat("*** Class Trajectories, method Show *** \n")
                  cat("* Times ="); print(object@times)
                  nrowShow <- min(10,nrow(object@traj))
                  ncolShow <- min(10,ncol(object@traj))
                  cat("* Traj (limited to a matrix 10x10) = \n")
                  print(formatC(object@traj[1:nrowShow,1:ncolShow]),quote=FALSE)
                  cat("******* End Show (trajectories) ******* \n")
                  }
                )
   
   trajStAnne
   
   
   # *** Class Trajectories, method Show *** 
   #   * Times = [1]  1  2  3  4  5  6  7  8  9 10 12 14 16 18 20 22 24 26 28 30 32
   # * Traj (limited to a matrix 10x10) = 
   #   [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9]  [,10]
   # [1,] 16.32 16.21 16.18 16.34 16.62 16.87 17.13 16.87 17.01 17.64
   # [2,] 16.07 15.64 16.34 16.33 16.59 16.75 17.24 17.05 17.07 17.13
   # [3,] 15.83 16.07 16.46 16.3  16.81 17.01 16.87 17.23 17.31 17.39
   # [4,] 15.85 16.19 16.3  16.73 16.43 16.82 16.77 16.7  17.29 17.7 
   # [5,] 15.99 16.14 16.09 16.32 16.88 16.53 16.85 17.03 17.16 17.38
   # [6,] 16.34 16.02 16.31 15.9  16.82 16.67 17.17 17.13 17.2  17.76
   # [7,] 16.23 15.88 16.19 16.59 16.94 16.45 17.27 17.28 17.26 17.37
   # [8,] 15.98 16.06 16.62 16.29 16.3  16.89 16.62 16.56 17.24 17.42
   # [9,] 15.87 15.96 16.45 16.42 16.67 16.69 16.99 17.04 17    17.42
   # [10,] 15.95 16.02 16.24 16.12 16.29 17.09 16.96 17.31 17.26 17.32
   # ******* End Show (trajectories) ******* 
   
   
   
   
   # More generally, all our methods must take into account the fact that they may have
   # to deal with the empty object:
   
   setMethod("show","Trajectories",
                function(object){
                  cat("*** Class Trajectories, method Show *** \n")
                  cat("* Times = "); print (object@times)
                  nrowShow <- min(10,nrow(object@traj))
                  ncolShow <- min(10,ncol(object@traj))
                  cat("* Traj (limited to a matrix 10x10) = \n")
                  if(length(object@traj)!=0){
                    print(formatC(object@traj[1:nrowShow,1:ncolShow]),quote=FALSE)
                    }else{}
                  cat("******* End Show (trajectories) ******* \n")
                  }
                )
   
   
   new("Trajectories")
   # *** Class Trajectories, method Show ***
   #   * Times = numeric(0)
   # * Traj (limited to a matrix 10x10) =
   #   ******* End Show (trajectories) *******
   
   
   
   #####################
   #####5.3 “setGeneric”
   ######################
   
   # Up to now, we did nothing but define methods which already existed (print existed
   #                                                                     for the numeric, for the character...) for the object Trajectories. We now need to
   # define a new method that is specific to Trajectories. Therefore, it is necessary for us
   # to declare it. This can be done by using the function setGeneric. This function requires
   # two arguments:
   
   
   # name is the name of the method which we define. 
   # def is an example of function which is used to define it.
   # It is not necessary to type the function. More precisely, it is not possible to type it,
   # it must be generic i.e. usable for several different classes.
   
   
   setGeneric (
      name= "countMissing",
      def=function(object){standardGeneric("countMissing")}
      )
   #[1] "countMissing"
   
   
   
   # This add countMissing to the list of the methods that R knows. We can now define
   # more specifically countMissing for the object trajectories:
      setMethod(
       f= "countMissing",
       signature= "Trajectories",
       definition=function(object){
         return(sum(is.na(object@traj)))
         }
       )
   #[1] "countMissing"
   
   
   ### To see the methods
   
    showMethods(class="Trajectories")
   # Function: countMissing (package .GlobalEnv)
   # object="Trajectories"
   # 
   # Function: initialize (package methods)
   # .Object="Trajectories"
   # (inherited from: .Object="ANY")
   # 
   # Function: plot (package graphics)
   # x="Trajectories"
   # 
   # Function: print (package base)
   # x="Trajectories"
   # 
   # Function: show (package methods)
   # object="Trajectories"
    
    
    
    getMethod(f="plot",signature="Trajectories")
    
    # Method Definition:
    #   
    #   function (x, y, ...) 
    #   {
    #     matplot(x@times, t(x@traj), xaxt = "n", type = "l", ylab = "", 
    #             xlab = "", pch = 1)
    #     axis(1, at = x@times)
    #   }
    # 
    # Signatures:
    #   x             
    # target  "Trajectories"
    # defined "Trajectories"
    
    existsMethod(f="plot",signature="Trajectories")
    # [1] TRUE
    
    
    ################
    ################Accessors
    
    ### Getter for "times"
    setGeneric("getTimes",function(object){standardGeneric ("getTimes")})
    
     setMethod("getTimes","Trajectories",
                 function(object){
                   return(object@times)
                   }
                 )
    
    
    getTimes(trajCochin)
    # [1] 1 2 4 5
    
     ### Getter for "traj"
    setGeneric("getTraj",function(object){standardGeneric("getTraj")})
    # [1] "getTraj"
     setMethod("getTraj","Trajectories",
                 function(object){
                   return(object@traj)
                   }
                 )
    
    
    getTraj(trajCochin)
    
    # [,1] [,2] [,3] [,4]
    # [1,] 15.0 15.1 15.2 15.2
    # [2,] 16.0 15.9 16.0 16.4
    # [3,] 15.2   NA 15.3 15.3
    # [4,] 15.7 15.6 15.8 16.0
    
    
    # But it is also possible to create more sophisticated getters. For example one can
    # regularly need the BMI at inclusion:
     ### Getter for the inclusion BMI (first column of "traj")
    
     setGeneric("getTrajInclusion",function(object){standardGeneric("getTrajInclusion")})
   
    setMethod ("getTrajInclusion","Trajectories",
                  function(object){
                    return(object@traj[,1])
                    }
                  )
    
    getTrajInclusion(trajCochin)
    # [1] 15.0 16.0 15.2 15.7
    
    
    ####SET
    # A setter is a method which assigns a value to a slot. With R, the assignment is made
    # by <-. Without entering the meanders of the program, the operator <- calls a specific
    # method. For example, when one uses names(data) <- c("A","B"), R calls the function
    # "names<-", this function duplicates the object data, modifies the attribute names of this
    # new object then overwrite data by this new object. We will do the same thing for the
    # slots of our trajectories. "setTime<-" will enable the modification of the slot times.
    # For this purpose, we shall use the function setReplaceMethod
    
    setGeneric("setTimes<-",function(object,value){standardGeneric("setTimes<-")})
    
    
    setReplaceMethod(
       f="setTimes",
       signature="Trajectories",
       definition=function(object,value){
         object@times <-value
         return (object)
         }
       )
    
    getTimes(trajCochin)
    # [1] 1 2 4 5
    setTimes(trajCochin) <- 1:3
    
    getTimes(trajCochin)
    # [1] 1 2 3
    
    
    
 
   # 7.3 The operator “[”   
    
    # It is also possible to define the getters by using the operator [. This can be made as for
    # an unspecified method by specifying the class and function to be applied. This function
    # takes for argument x (the object), i and j (which will be between “[” and “]”) and drop.
    # i will indicate the field which we want to reach. If the slot is a complex object (a matrix,
    #  a list,...), j will make it possible to specify a particular element.
    
    setMethod(
       f= "[",
       signature="Trajectories",
       definition=function(x,i,j,drop){
         if(i=="times"){return(x@times)}else {}
         if(i=="traj"){return(x@traj)}else {}
         }
       )
    
    
    trajCochin["times"]
    # [1] 1 2 3
    
    trajCochin["traj"]
    
    # [,1] [,2] [,3] [,4]
    # [1,] 15.0 15.1 15.2 15.2
    # [2,] 16.0 15.9 16.0 16.4
    # [3,] 15.2   NA 15.3 15.3
    # [4,] 15.7 15.6 15.8 16.0
