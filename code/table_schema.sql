-- Search Page Table Schema
CREATE TABLE scrape_plan (
    scrape_id SERIAL,
    city varchar(20),
    start_date date,
    end_date date,
    pages int,
    number_of_people int,
    scrape_time time
);

CREATE TABLE search_page (
    scrape_id int,
    listing_id varchar(20),
    url varchar(50),
    page_number int,
    page_counter int,
    price int,
    reviews int
);

CREATE TABLE detail_page(
    scrape_id int,
    listing_id varchar(20),
    listing_date date,
    price int,
    availability bool
);
