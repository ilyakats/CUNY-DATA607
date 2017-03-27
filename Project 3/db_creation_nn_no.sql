# Nkasi Nedd
# Nnaemezue Obi-Eyisi

CREATE SCHEMA IF NOT EXISTS datascienceskills;

USE datascienceskills;

DROP TABLE IF EXISTS skillsdata;
DROP TABLE IF EXISTS Skills;
DROP TABLE IF EXISTS Categories;Location
DROP TABLE IF EXISTS Location;

# Categories table to store information about categories of skills.
CREATE TABLE Categories 
	(CategoryID INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
Description VARCHAR(20) NOT NULL);
     
# Skills table to store data science skills.
CREATE TABLE Skills
	(SkillID INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
SkillDescription VARCHAR(15) NOT NULL,
SkillCategory INT,
CONSTRAINT FOREIGN KEY (SkillCategory) 
		REFERENCES Categories (CategoryID)
		ON DELETE SET NULL);

# Location table to store data about where jobs are found.
CREATE TABLE Location 
	(LocationID INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
Description VARCHAR(20) NOT NULL,
Country VARCHAR (15) NOT NULL);
     
# SkillsData table to store data about skills found.
CREATE TABLE skillsdata
	(SkillsDataID INT AUTO_INCREMENT PRIMARY KEY NOT NULL,
Place INT,
Skill INT,
Amount INT,
Rating FLOAT,
YearCollected INT,
CONSTRAINT FOREIGN KEY (Skill)
REFERENCES Skills (SkillID)
ON DELETE SET NULL,
CONSTRAINT FOREIGN KEY (Place)
REFERENCES Location (LocationID)
ON DELETE SET NULL);
