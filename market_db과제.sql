DROP DATABASE IF EXISTS market_db; -- 만약 market_db가 존재하면 우선 삭제한다.
CREATE DATABASE market_db;

USE market_db;
CREATE TABLE member -- 회원 테이블
( mem_id  		CHAR(8) NOT NULL PRIMARY KEY, -- 사용자 아이디(PK)
  mem_name    	VARCHAR(10) NOT NULL, -- 이름
  mem_number    INT NOT NULL,  -- 인원수
  addr	  		CHAR(2) NOT NULL, -- 지역(경기,서울,경남 식으로 2글자만입력)
  phone1		CHAR(3), -- 연락처의 국번(02, 031, 055 등)
  phone2		CHAR(8), -- 연락처의 나머지 전화번호(하이픈제외)
  height    	SMALLINT,  -- 평균 키
  debut_date	DATE  -- 데뷔 일자
);
CREATE TABLE buy -- 구매 테이블
(  num 		INT AUTO_INCREMENT NOT NULL PRIMARY KEY, -- 순번(PK)
   mem_id  	CHAR(8) NOT NULL, -- 아이디(FK)
   prod_name 	CHAR(6) NOT NULL, --  제품이름
   group_name 	CHAR(4)  , -- 분류
   price     	INT  NOT NULL, -- 가격
   amount    	SMALLINT  NOT NULL, -- 수량
   FOREIGN KEY (mem_id) REFERENCES member(mem_id)
);

INSERT INTO member VALUES('TWC', '트와이스', 9, '서울', '02', '11111111', 167, '2015.10.19');
INSERT INTO member VALUES('BLK', '블랙핑크', 4, '경남', '055', '22222222', 163, '2016.08.08');
INSERT INTO member VALUES('WMN', '여자친구', 6, '경기', '031', '33333333', 166, '2015.01.15');
INSERT INTO member VALUES('OMY', '오마이걸', 7, '서울', NULL, NULL, 160, '2015.04.21');
INSERT INTO member VALUES('GRL', '소녀시대', 8, '서울', '02', '44444444', 168, '2007.08.02');
INSERT INTO member VALUES('ITZ', '잇지', 5, '경남', NULL, NULL, 167, '2019.02.12');
INSERT INTO member VALUES('RED', '레드벨벳', 4, '경북', '054', '55555555', 161, '2014.08.01');
INSERT INTO member VALUES('APN', '에이핑크', 6, '경기', '031', '77777777', 164, '2011.02.10');
INSERT INTO member VALUES('SPC', '우주소녀', 13, '서울', '02', '88888888', 162, '2016.02.25');
INSERT INTO member VALUES('MMU', '마마무', 4, '전남', '061', '99999999', 165, '2014.06.19');

INSERT INTO buy VALUES(NULL, 'BLK', '지갑', NULL, 30, 2);
INSERT INTO buy VALUES(NULL, 'BLK', '맥북프로', '디지털', 1000, 1);
INSERT INTO buy VALUES(NULL, 'APN', '아이폰', '디지털', 200, 1);
INSERT INTO buy VALUES(NULL, 'MMU', '아이폰', '디지털', 200, 5);
INSERT INTO buy VALUES(NULL, 'BLK', '청바지', '패션', 50, 3);
INSERT INTO buy VALUES(NULL, 'MMU', '에어팟', '디지털', 80, 10);
INSERT INTO buy VALUES(NULL, 'GRL', '혼공SQL', '서적', 15, 5);
INSERT INTO buy VALUES(NULL, 'APN', '혼공SQL', '서적', 15, 2);
INSERT INTO buy VALUES(NULL, 'APN', '청바지', '패션', 50, 1);
INSERT INTO buy VALUES(NULL, 'MMU', '지갑', NULL, 30, 1);
INSERT INTO buy VALUES(NULL, 'APN', '혼공SQL', '서적', 15, 1);
INSERT INTO buy VALUES(NULL, 'MMU', '지갑', NULL, 30, 4);

SELECT * FROM member;
SELECT * FROM buy;

-- 프로시저 1번
USE market_db;
DROP PROCEDURE IF EXISTS user_proc1;
DELIMITER $$
CREATE PROCEDURE user_proc1(IN mname VARCHAR(10))
BEGIN 
   SELECT mem_name, mem_number, debut_date FROM member WHERE mem_name = mname;
END $$ 
DELIMITER ;

CALL user_proc1('에이핑크');

-- 프로시저 2번
DROP PROCEDURE IF EXISTS user_proc2;
DELIMITER $$ 
CREATE PROCEDURE user_proc2( 
     IN memnum INT, 
     IN memheight INT 
) 
BEGIN 
   SELECT * FROM member
     WHERE mem_number > memnum AND height > memHeight;
END $$ 
DELIMITER ;
 
CALL user_proc2(6, 165);

-- 프로시저 3번
DROP PROCEDURE IF EXISTS message_proc;
DELIMITER $$ 
CREATE PROCEDURE message_proc( 
     IN mem_n VARCHAR(10) 
) 
BEGIN 
     DECLARE dYear INT; -- 변수 선언 
     SELECT YEAR(debut_date) into dYear FROM member 
         WHERE mem_Name = mem_n;
     IF (dYear >= 2015) THEN 
            SELECT '신인가수네요. 화이팅하세요';
     ELSE 
            SELECT '고참가수네요. 그동안 수고하셨어요';
     END IF;
END $$ 
DELIMITER ;

CALL message_proc('오마이걸');

-- 프로시저 4번
DROP PROCEDURE IF EXISTS avg_member;
DELIMITER $$
CREATE PROCEDURE avg_member() 
BEGIN 
    DECLARE avg_num FLOAT;
    SELECT AVG(mem_number) INTO avg_num FROM member;
    SELECT avg_num AS average_count;
END $$ 
DELIMITER ;

CALL avg_member();


set sql_safe_updates=0;
-- 트리거 1번
CREATE TABLE singer 
             (SELECT mem_id, mem_name, mem_number, addr 
               FROM member);

CREATE TABLE backup_singer( mem_id 		CHAR(8) NOT NULL ,   
							mem_name    VARCHAR(10) NOT NULL,  
							mem_number  INT NOT NULL,   
							addr	    CHAR(2) NOT NULL,  
							modType     CHAR(2), -- 변경된 타입. '수정' 또는 '삭제' 
							modDate		DATE, -- 변경된 날짜  
							modUser		VARCHAR(30));


DROP TRIGGER IF EXISTS singer_update_Trg;
DELIMITER $$
CREATE TRIGGER singer_update_Trg
AFTER UPDATE ON singer
FOR EACH ROW
BEGIN
    INSERT INTO backup_singer (mem_id, mem_name, mem_number, addr, modType, modDate, modUser)
    VALUES (NEW.mem_id, NEW.mem_name, NEW.mem_number, NEW.addr, '수정', CURDATE(), 'SYSTEM');
END $$ 
DELIMITER ;

DROP TRIGGER IF EXISTS singer_delete_Trg;
DELIMITER $$
CREATE TRIGGER singer_delete_Trg
AFTER DELETE ON singer
FOR EACH ROW
BEGIN
    INSERT INTO backup_singer (mem_id, mem_name, mem_number, addr, modType, modDate, modUser)
    VALUES (OLD.mem_id, OLD.mem_name, OLD.mem_number, OLD.addr, '삭제', CURDATE(), 'SYSTEM');
END $$ 
DELIMITER ;

UPDATE singer SET addr = '영국' WHERE mem_id = 'BLK';
SELECT * FROM backup_singer;

Delete from singer where mem_number >= 7;
SELECT * FROM backup_singer;