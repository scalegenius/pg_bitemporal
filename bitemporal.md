
## pg_bitemporal

Bitemporal Table support in Postgresql 9.4.



## Bitemporal Terms

* Bitemporal Table is a relational database table with two time periods from
  two temporal dimensions per row. 
  In the standard theory, these dimensions are called valid time and 
  transaction time. In the Asserted Versioning theory, these dimensions are 
  state time and assertion time.

* Effective Time Period is the temporal dimension within which effective 
  time periods are located. Each row in a bitemporal table has  
  an effective time period. 
  An effective time period is the temporal interval during which 
  some row has, had or will have the properties and/or relationships
  related to the row.
  The standard theory calls this valid time.

* Row Creation Time is a point in time to record when a given row is inserted
  into a table. 

* Assertion Time is the temporal dimension within which assertion-time periods
  are located. 
  Each row in a bitemporal table has an assertion-time period. 
  An assertion-time period is a time period during which a statement 
  was, is or will be asserted to be true.

* Time Interval is an unachored, directional duration, contiguous 
  portion of the time line. An Interval is relative. 
  For example: '3 months' '1 year 2 days'

* Time Period is a period of time deliniated by a beginning time instance
  and an end time instance. A Time period is displayed using Closed-Open
  range syntax [begin, end ). Begin is inclusive in the range while End is
  exclusive to the range. If end is the inclusive than an single step value
  should be added to the inclusive end to find the period end 
  Synonyms: Time Range 

* Clock Tick is the smallest point in time, at a given level of granularity,
  used for a given set of time periods. It is used to calculated the start
  of the next continious time period.
  

* Allen relationships are a set of thirteen positional relationships between
  two time periods, as first defined in James F. Allen's 1983 article
  “Maintaining Knowledge about Temporal Intervals”. Every possible positional
  relationship between two time periods is represented by one and only one
  member of this set.


* Temporal Primary Key is the primary key of a temporal table. It is
  usually a natural key plus the effective and asserted time periods.


* Temporal Referential Integrity is a standard referential integrity 
  constraint with a time dimensions. For example a foreign key RI between
  two bitemporal tables.
  Temporal referential integrity is the constraint that the bitemporal 
  extent of every row about a thing is [contained in] the bitemporal 
  extent of a set of one or more rows has an existence dependency.




## What Bitemporal Combinations mean

|   | what we used to claim | what we currently claim | what we will claim|
|---|-----------------------|-------------------------|------------------|
|what things used to be like| what we used to claim things used to be like| what we currently claim things used to be like| what we will claim things used to be like|
|what things are like | what we used to claim things are like now | what we currently claim things are like now | what we will claim things are like now|
|what things will be like| what we used to claim things will be like| what we currently claim things will be like| what we will claim things will be like |

