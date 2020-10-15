SELECT * FROM bitemporal_internal.ll_create_bitemporal_table(
    'bt_tutorial',
    'staff_bt',
	$$staff_id int, 
	  staff_name TEXT NOT NULL,
      staff_location TEXT NOT NULL
	$$,
   'staff_id');
   
 SELECT * FROM bitemporal_internal.ll_create_bitemporal_table(
    'bt_tutorial',
    'cust_bt',
	$$cust_id int NOT NULL, 
	  cust_name TEXT NOT NULL,
      phone TEXT
	$$,
   'cust_id');
   
   SELECT * FROM bitemporal_internal.ll_create_bitemporal_table(
    'bt_tutorial',
    'product_bt',
	$$product_id INT,
	  product_name text NOT NULL,
      weight INTEGER NOT NULL DEFAULT(0),
      price INTEGER NOT NULL DEFAULT(0)
	$$,
   'product_id');
   
 SELECT * FROM bitemporal_internal.ll_create_bitemporal_table(
    'bt_tutorial',
    'order_bt',
	$$order_id INT NOT NULL,
	  staff_id INT NOT NULL,
          cust_id INT NOT NULL,
	  order_created_at timestamptz
	$$,
   'order_id');
     
 SELECT * FROM bitemporal_internal.ll_create_bitemporal_table(
    'bt_tutorial',
    'order_line_bt',
	$$order_line_id INT NOT NULL,
	 order_id INT NOT NULL,
          product_id INT NOT NULL,
	 qty int NOT NULL,
           order_line_created_at timestamptz
	$$,
   'order_id,order_line_id');
  
   
  
drop SEQUENCE if exists bt_tutorial.staff_id_seq;
CREATE SEQUENCE bt_tutorial.staff_id_seq;
drop SEQUENCE if exists bt_tutorial.cust_id_seq;
CREATE SEQUENCE bt_tutorial.cust_id_seq;
drop SEQUENCE if exists bt_tutorial.product_id_seq;
CREATE SEQUENCE bt_tutorial.product_id_seq;
drop SEQUENCE if exists bt_tutorial.order_id_seq;
CREATE SEQUENCE bt_tutorial.order_id_seq;
drop SEQUENCE if exists bt_tutorial.order_line_id_seq;
CREATE SEQUENCE bt_tutorial.order_line_id_seq;


select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.staff_bt'
,$$staff_id, staff_name, staff_location$$
,quote_literal(nextval('staff_id_seq'))||$$,
'mystaff', 'mylocation'$$
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.cust_bt'
,$$cust_id, cust_name, phone$$
,quote_literal(nextval('cust_id_seq'))||$$,
'mycust', '+6281197889890'$$
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.product_bt'
,$$product_id, product_name,weight,price$$
,quote_literal(nextval('product_id_seq'))||$$,
'myproduct', 100,200$$
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.product_bt'
,$$product_id, product_name,weight,price$$
,quote_literal(nextval('product_id_seq'))||$$,
'myproduct2', 200,250$$
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.order_bt'
,$$order_id, staff_id,cust_id,order_created_at$$
,quote_literal(nextval('order_id_seq'))||$$,
1,1,$$||quote_literal(now())
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.order_line_bt'
,$$order_line_id,order_id, product_id,qty$$
,quote_literal(nextval('order_line_id_seq'))||$$,
1,1,10$$
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

select * from bitemporal_internal.ll_bitemporal_insert('bt_tutorial.order_line_bt'
,$$order_line_id,order_id, product_id,qty$$
,quote_literal(nextval('order_line_id_seq'))||$$,
1,2,15$$
,temporal_relationships.timeperiod(now(), 'infinity') --effective
,temporal_relationships.timeperiod(now(), 'infinity') --asserted
);

SELECT * FROM  bitemporal_internal.ll_bitemporal_update('bt_tutorial'
,'staff_bt'
,'staff_location'-- fields to update'
,$$'newlocation'$$  -- values to update with
,'staff_id'  -- search fields
,'1' --  search values
,temporal_relationships.timeperiod(now(), 'infinity')
,temporal_relationships.timeperiod(now(), 'infinity')
) ;

select
o.order_id, 
staff_name, 
staff_location, 
c.cust_name, 
c.phone AS cust_phone, 
p.product_name, 
p.price,
l.qty
    FROM order_line_bt l
    JOIN order_bt o ON o.order_id = l.order_id
    JOIN product_bt p ON p.product_id = l.product_id
    JOIN staff_bt s ON s.staff_id = o.staff_id
    JOIN cust_bt c ON c.cust_id = o.cust_id
WHERE l.order_id=1
AND order_line_created_at<@l.effective AND now()<@l.asserted
AND order_created_at<@o.effective AND now()<@o.asserted
AND order_created_at<@c.effective AND now()<@c.asserted
AND order_created_at<@p.effective AND now()<@p.asserted
AND order_created_at<@s.effective AND now()<@s.asserted;


SELECT * FROM bitemporal_internal.ll_bitemporal_correction(
    'bt_tutorial',
	'product_bt',
    'price',
   '275',
    'product_id',
    '2',
    temporal_relationships.timeperiod ('2020-10-11 18:33:26.816311-05'::timestamptz,'infinity'),
    now() );
    
    ---corrected price
select
o.order_id, 
staff_name, 
staff_location, 
c.cust_name, 
c.phone AS cust_phone, 
p.product_name, 
p.price,
l.qty
    FROM order_line_bt l
    JOIN order_bt o ON o.order_id = l.order_id
    JOIN product_bt p ON p.product_id = l.product_id
    JOIN staff_bt s ON s.staff_id = o.staff_id
    JOIN cust_bt c ON c.cust_id = o.cust_id
WHERE l.order_id=1
AND order_line_created_at<@l.effective AND now()<@l.asserted
AND order_created_at<@o.effective AND now()<@o.asserted
AND order_created_at<@c.effective AND now()<@c.asserted
AND order_created_at<@p.effective AND now()<@p.asserted
AND order_created_at<@s.effective AND now()<@s.asserted;

--original price

select
o.order_id, 
staff_name, 
staff_location, 
c.cust_name, 
c.phone AS cust_phone, 
p.product_name, 
p.price,
l.qty
    FROM order_line_bt l
    JOIN order_bt o ON o.order_id = l.order_id
    JOIN product_bt p ON p.product_id = l.product_id
    JOIN staff_bt s ON s.staff_id = o.staff_id
    JOIN cust_bt c ON c.cust_id = o.cust_id
WHERE l.order_id=1
AND order_line_created_at<@l.effective AND order_line_created_at<@l.asserted
AND order_created_at<@o.effective AND order_created_at<@o.asserted
AND order_created_at<@c.effective AND order_created_at<@c.asserted
AND order_created_at<@p.effective AND order_created_at<@p.asserted
AND order_created_at<@s.effective AND order_created_at<@s.asserted;