-- MySQL dump 10.13  Distrib 5.5.24, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: crm
-- ------------------------------------------------------
-- Server version	5.5.24-0ubuntu0.12.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `company`
--

DROP TABLE IF EXISTS `company`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `company` (
  `company_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `names` varchar(500) COLLATE utf8_czech_ci NOT NULL,
  `address` varchar(500) COLLATE utf8_czech_ci DEFAULT NULL,
  `show_in_index` tinyint(1) DEFAULT NULL,
  `priority_days` int(10) unsigned DEFAULT NULL,
  `delay` varchar(100) COLLATE utf8_czech_ci DEFAULT NULL,
  PRIMARY KEY (`company_id`)
) ENGINE=InnoDB AUTO_INCREMENT=12 DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `company`
--

LOCK TABLES `company` WRITE;
/*!40000 ALTER TABLE `company` DISABLE KEYS */;
INSERT INTO `company` VALUES (1,'AIR PRODUCTS spol. s r.o.','Mnichovo Hradiště',1,0,NULL),(2,'ALIVA','Radvanice',1,0,NULL),(3,'Alutec K + K','Čelákovice ',1,0,NULL),(4,'AmTec Defense s.r.o.','Praha 9 ',1,0,NULL),(5,'Angelo Decoration ','Cheb ',1,0,NULL),(6,'ASTRON print','Praha 9 ',1,0,NULL),(7,'ADVANTAGE CARS s.r.o. ','Praha  ',1,0,NULL),(8,'Auto Pelc','Praha H. Počernice',1,0,NULL),(9,'Auto Tichý ','Lysá nad Labem ',1,0,NULL),(10,'AUTOZAM Baláš s.r.o.','Město Albrechtice ',1,0,NULL),(11,'AIR PRODUCTS spol. s r.o.','Žebrák',1,0,NULL);
/*!40000 ALTER TABLE `company` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `person`
--

DROP TABLE IF EXISTS `person`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `person` (
  `person_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `position` varchar(50) COLLATE utf8_czech_ci DEFAULT NULL,
  `telephone` varchar(50) COLLATE utf8_czech_ci DEFAULT NULL,
  `cell_phone` varchar(50) COLLATE utf8_czech_ci DEFAULT NULL,
  `fax` varchar(50) COLLATE utf8_czech_ci DEFAULT NULL,
  `mail` varchar(100) COLLATE utf8_czech_ci DEFAULT NULL,
  `company_id` int(10) unsigned DEFAULT NULL,
  `is_serviceman` tinyint(1) NOT NULL,
  `name` varchar(200) COLLATE utf8_czech_ci NOT NULL,
  PRIMARY KEY (`person_id`),
  KEY `company_id` (`company_id`),
  CONSTRAINT `person_ibfk_1` FOREIGN KEY (`company_id`) REFERENCES `company` (`company_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `person`
--

LOCK TABLES `person` WRITE;
/*!40000 ALTER TABLE `person` DISABLE KEYS */;
/*!40000 ALTER TABLE `person` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `service`
--

DROP TABLE IF EXISTS `service`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `service` (
  `service_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `date1` date NOT NULL,
  `type1` char(1) COLLATE utf8_czech_ci NOT NULL,
  `result` varchar(1000) COLLATE utf8_czech_ci DEFAULT NULL,
  PRIMARY KEY (`service_id`)
) ENGINE=InnoDB AUTO_INCREMENT=54 DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `service`
--

LOCK TABLES `service` WRITE;
/*!40000 ALTER TABLE `service` DISABLE KEYS */;
INSERT INTO `service` VALUES (36,'2005-04-29','n',''),(37,'2008-04-01','n',''),(38,'2009-02-25','n',''),(39,'2010-02-16','n',''),(40,'2011-02-28','n','ok'),(41,'2012-02-07','n','ok'),(42,'2011-03-25','n','ok'),(43,'2012-05-17','n','termostat'),(44,'2009-08-13','n','ok'),(45,'2011-08-18','n','vložka'),(46,'2012-06-25','n',''),(47,'2012-07-26','n',''),(48,'2012-08-29','n','ok'),(49,'2012-10-15','n','červeně'),(50,'2011-01-14','n','ok'),(51,'2012-01-27','n','ok'),(52,'2012-09-03','n','EO, Olej'),(53,'2011-10-17','n','ok');
/*!40000 ALTER TABLE `service` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `service_part`
--

DROP TABLE IF EXISTS `service_part`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `service_part` (
  `service_id` int(10) unsigned NOT NULL DEFAULT '0',
  `machine_hours` int(10) unsigned DEFAULT NULL,
  `serviceable_id` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`service_id`,`serviceable_id`),
  KEY `serviceable_id` (`serviceable_id`),
  CONSTRAINT `service_part_ibfk_1` FOREIGN KEY (`service_id`) REFERENCES `service` (`service_id`),
  CONSTRAINT `service_part_ibfk_2` FOREIGN KEY (`serviceable_id`) REFERENCES `serviceable` (`serviceable_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `service_part`
--

LOCK TABLES `service_part` WRITE;
/*!40000 ALTER TABLE `service_part` DISABLE KEYS */;
INSERT INTO `service_part` VALUES (36,NULL,3),(37,NULL,3),(38,NULL,3),(39,NULL,3),(40,NULL,3),(41,NULL,3),(42,NULL,2),(43,NULL,2),(44,NULL,8),(45,NULL,8),(46,NULL,8),(47,NULL,1),(48,NULL,10),(49,NULL,5),(50,NULL,12),(51,NULL,12),(52,NULL,12),(53,NULL,6),(53,NULL,7);
/*!40000 ALTER TABLE `service_part` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `serviceable`
--

DROP TABLE IF EXISTS `serviceable`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `serviceable` (
  `serviceable_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `parent_id` int(10) unsigned DEFAULT NULL,
  `company_id` int(10) unsigned DEFAULT NULL,
  `manufacturer` varchar(100) COLLATE utf8_czech_ci NOT NULL,
  `into_service` date DEFAULT NULL,
  `date_sold` date DEFAULT NULL,
  `serial_number` varchar(100) COLLATE utf8_czech_ci DEFAULT NULL,
  `power` double DEFAULT NULL,
  `type1` varchar(100) COLLATE utf8_czech_ci DEFAULT NULL,
  `note` varchar(2000) COLLATE utf8_czech_ci NOT NULL,
  `specification` varchar(100) COLLATE utf8_czech_ci NOT NULL,
  `interval_days` int(11) DEFAULT NULL,
  PRIMARY KEY (`serviceable_id`),
  KEY `company_id` (`company_id`),
  KEY `parent_id` (`parent_id`),
  CONSTRAINT `serviceable_ibfk_1` FOREIGN KEY (`company_id`) REFERENCES `company` (`company_id`),
  CONSTRAINT `serviceable_ibfk_2` FOREIGN KEY (`parent_id`) REFERENCES `serviceable` (`serviceable_id`)
) ENGINE=InnoDB AUTO_INCREMENT=13 DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `serviceable`
--

LOCK TABLES `serviceable` WRITE;
/*!40000 ALTER TABLE `serviceable` DISABLE KEYS */;
INSERT INTO `serviceable` VALUES (1,NULL,1,'Atlas Copco',NULL,NULL,'S99305601',NULL,'','jsou tam 2','GA 160FF',365),(2,NULL,2,'CompAir',NULL,NULL,'349012/940',NULL,'','','Start 025',365),(3,NULL,3,'Remeza',NULL,NULL,'3',NULL,'','','BK 7A-10-270',365),(4,NULL,4,'Atmos',NULL,NULL,NULL,NULL,'','','E 80 Vario',365),(5,NULL,5,'Remeza',NULL,NULL,'1736',NULL,'','','BK 15E',365),(6,NULL,6,'Ingersoll Rand',NULL,NULL,'2146612',NULL,'','','Unigy',365),(7,NULL,6,'Remeza','2010-07-12',NULL,'0125',NULL,'','','BK 25-10',365),(8,NULL,8,'CompAir ',NULL,NULL,'349003/2773 ',NULL,'','','Start 0101 ',365),(9,NULL,10,'Atmos','2012-01-31',NULL,'90057',NULL,'','','E 170 KV',365),(10,NULL,11,'Atlas Copco',NULL,NULL,'ADF 141347',NULL,'','počet:2','ZT 132 MOD',365),(11,NULL,9,'Atmos','2001-01-01',NULL,'79002',NULL,'','','E 50 V',365),(12,NULL,7,'Atlas Copco ',NULL,NULL,'AII 625744 ',NULL,'','','GX 7 ',365);
/*!40000 ALTER TABLE `serviceable` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `serviceman`
--

DROP TABLE IF EXISTS `serviceman`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `serviceman` (
  `serviceman_id` int(10) unsigned NOT NULL DEFAULT '0',
  `service_id` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`service_id`,`serviceman_id`),
  KEY `serviceman_id` (`serviceman_id`),
  CONSTRAINT `serviceman_ibfk_1` FOREIGN KEY (`service_id`) REFERENCES `service` (`service_id`),
  CONSTRAINT `serviceman_ibfk_2` FOREIGN KEY (`serviceman_id`) REFERENCES `person` (`person_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `serviceman`
--

LOCK TABLES `serviceman` WRITE;
/*!40000 ALTER TABLE `serviceman` DISABLE KEYS */;
/*!40000 ALTER TABLE `serviceman` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-10-18 18:04:05
