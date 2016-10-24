create trigger t_RI_bt_insert 
BEFORE INSERT OR UPDATE  ON  bi_temp_tables.app_devices_test
    FOR EACH ROW
     EXECUTE PROCEDURE bitemporal_internal.RI_BT_FKey_check_ins('bi_temp_tables',
       'app_devices_test', 'device_id');
       