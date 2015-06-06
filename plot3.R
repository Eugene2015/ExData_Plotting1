# Function that builds 3th plot
# Parameters :
#             dir.working      [optional] - folder that will contain generated image
#             output.file.name [optional] - name of the output file
#             image.width      [optional] - width of the image to be generated
#             image.height     [optional] - height of the image to be generated
plot3<-function(dir.working = "./", output.file.name="plot3.png", image.width=480, image.height=480){
    
    subfolder = "./dataset"
    
    archive.file.name = sprintf("%s/archive.zip", subfolder);
    dataset.file.name = sprintf("%s/household_power_consumption.txt", subfolder);
    
    # save current and set working directory     
    dir.current<-getwd();
    setwd(dir.working);
    
    # the very first step - make sure that dataset file exists 
    if(!file.exists(dataset.file.name)){
        
        # file does not exists. download one        
        # -- dataset will be put into subdirectory. create one if necessary 
        if(!file.exists(subfolder)){
            dir.create(subfolder);
        }
        
        # --  download archive file that contains dataset
        download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", dest=archive.file.name, method="curl", quiet=T);
        if(!file.exists(archive.file.name)){
            stop("unable to download dataset archive file.");
        }
        print(getwd());
        
        # -- extract file from archive and verify that extraction succeeded
        unzip(archive.file.name, junkpaths = T, exdir=subfolder);
        if(!file.exists(dataset.file.name)){
            stop("unable to unzip dataset archive file");
        }
        
        # -- no need for archive file any more
        unlink(archive.file.name);
    }
    
    # second step - load dataset internally
    # -- first determine column classes. 
    dataset <- read.table(dataset.file.name, header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = F, nrows = 10);
    classes <- sapply(dataset, class);
    dsnames <- names(dataset);
    
    # -- read data into dataset
    # -- there are number of ways to read just certain lines of the dataset inti data frame.
    #    one of the approaches is to use sqldf package. read.cvs.sql allows to specify sql statement according to which data will be loaded.
    #    this approach is suitable for more complex cases than this one as sqldf will create sqlite database, move all dataset data into the 
    #    database and after that will run specified sql query against that database to fetch only data requested.
    #    it will be way faste and less resourse consuming to take advantage of the fact that whole dataset is sorted by date and time and read.table
    #    function allows to specify how many rows to skip and how many rows to read
    #    So using standard unix commands we can find number of first record for 1/1/2007, total number of records for 1/1/2007 and 2/1/2007.     
    
    # -- -- number of rows to skip - command below will return number of the first line that contains '1/2/2007;' in file as first value
    cmd<-sprintf("grep -n -m 1 '^1\\/2\\/2007;' %s", dataset.name)    
    rows.to.skip = as.integer(strsplit(system(cmd, intern=T), ":")[[1]][1]) - 2; # adjustment that takes into consideratiion column names row
    
    # -- -- number of rows to read : (grep -c '^1\/2\/2007;' household_power_consumption.txt) + (grep -c '^2\/2\/2007;' household_power_consumption.txt)
    cmd<-sprintf("grep -c '^1\\/2\\/2007;' %s", dataset.name); 
    rows.to.read <- as.integer(system(cmd, intern = T)); 
    cmd<-sprintf("grep -c '^2\\/2\\/2007;' %s", dataset.name);
    rows.to.read <- rows.to.read +  as.integer(system(cmd, intern = T)); 
    
    # -- -- read it!
    dataset <- read.table(dataset.file.name, header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = F, colClasses = classes, skip=rows.to.skip, nrows=rows.to.read);
    
    # -- set/restore column names     
    names(dataset)<-dsnames;
    
    # again, there are multiple ways to deal with date and time here. one of the ways is to combine Date and Time variables into
    # single vector of dates and use it
    timestamp<-strptime(paste(ds$Date, ds$Time), "%d/%m/%Y %H:%M:%S");
        
    # generate image -- add three variables to the same plot
    png(output.file.name, width=image.width, height=image.height, bg="transparent");   
    
    with(dataset, plot(timestamp, Sub_metering_1, type = "n", ylab="Energy sub metering", xlab="") );
    with(dataset, lines(timestamp, Sub_metering_1, col = "black"));
    with(dataset, lines(timestamp, Sub_metering_2, col = "red"));
    with(dataset, lines(timestamp, Sub_metering_3, col = "blue"));
        
    legend("topright",  
           c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), # puts text in the legend           
           # give the legend appropriate symbols (lines)
           lty = c(1, 1),
           # give the legend lines the correct color and width    
           lwd = c(2.5, 2.5, 2.5),
           col = c("black", "red", "blue")); 
    
    dev.off();
    
    # restore original working directory
    setwd(dir.current);
    
    # build return value
    paste(dir.working, output.file.name, sep="");
    normalizePath(paste(dir.working, output.file.name, sep=""));
}
