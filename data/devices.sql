create table devices (device_name text unique not null, url text, device_owner text, comments text, reservation_status text);
insert into devices values ("DEVICE-1", "https://source/browse/dev1", "Toby", "no comment", "reserved");
insert into devices values ("DEVICE-2", "https://source/browse/dev2", NULL, NULL, "available");
insert into devices values ("DEVICE-3", "https://source/browse/dev3", "", NULL, "available");
insert into devices values ("DEVICE-4", "https://source/browse/dev4", "Toby", "no comment", "reserved");
