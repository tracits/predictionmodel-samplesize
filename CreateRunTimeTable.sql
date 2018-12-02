select * from runtime_data order by id asc LIMIT 1
#truncate table runtime_data
#2018-11-21 13:03:13     1
CREATE TABLE `runtime_data` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `executionID` varchar(14) DEFAULT NULL,
  `loopCount` int(11) DEFAULT NULL,
  `developmentprevalence` decimal(10,0) DEFAULT NULL,
  `updatingvalidationprevalence` decimal(10,0) DEFAULT NULL,
  `numberofdevelopmentnonevents` int(11) DEFAULT NULL,
  `numberofvalidationnonevents` int(11) DEFAULT NULL,
  `numberofupdatingnonevents` int(11) DEFAULT NULL,
  `modelMIntercept` decimal(10,0) DEFAULT NULL,
  `modelMSBP` decimal(10,0) DEFAULT NULL,
  `modelMPULSE` decimal(10,0) DEFAULT NULL,
  `modelMRR` decimal(10,0) DEFAULT NULL,
  `modelMGCSTOT` decimal(10,0) DEFAULT NULL,
  `modelUMIntercept` decimal(10,0) DEFAULT NULL,
  `comparisonResult` decimal(10,0) DEFAULT NULL,
  `regdate` datetime DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=923737 DEFAULT CHARSET=utf8mb4;
