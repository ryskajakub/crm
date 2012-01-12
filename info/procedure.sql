DELIMITER $$

CREATE TRIGGER either_company_or_parent BEFORE INSERT ON a FOR EACH ROW
BEGIN
  DECLARE c_id INTEGER;
  DECLARE p_id INTEGER;
  SELECT NEW.company_id INTO c_id FROM a;
  SELECT NEW.parent_id INTO p_id FROM a;
  IF (c_id IS NULL AND p_id IS NULL) OR (c_id IS NOT NULL AND p_id IS NOT NULL) THEN
    SELECT error 
    FROM `only a or b`;
  END IF;

END $$

DELIMITER ;

