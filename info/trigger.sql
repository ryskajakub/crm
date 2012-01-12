DELIMITER $$

CREATE TRIGGER ensure_company_parent BEFORE INSERT ON serviceable FOR EACH ROW
BEGIN
  DECLARE error varchar(255);
  IF ((NEW.company_id is null and NEW.parent_id is null) or (NEW.company_id is not null and NEW.parent_id is not null)) THEN
	 select error into error from `$$$ERROR_MESSAGEE:Exactly one of parent_id or company_id must be non null$$$`;
  END IF;

END $$

DELIMITER ;

