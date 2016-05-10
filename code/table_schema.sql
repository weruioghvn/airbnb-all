-- Search Page Table Schema
CREATE TABLE search_page (
    listing_id varchar(20),
    url varchar(50),
    page_number int,
    page_counter int,
    start_date date,
    end_date date,
    number_of_people int,
    city varchar(20),
    scrape_time time
);
    
CREATE TABLE listing_table (
    listing_id varchar(20),
    city varchar(20)
);

CREATE TABLE detail_page (
    listing_id varchar(20),
    host_id varchar(20),
);
