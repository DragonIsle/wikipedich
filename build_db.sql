create table keyboard_buttons
(
	keyboard_id int not null,
	row_order int not null,
	button_order int not null,
	text varchar not null,
	url varchar,
	callback_data varchar,
	constraint keyboard_buttons_pk
		primary key (keyboard_id, row_order, button_order)
);

create table callback_messages
(
	cq_data varchar not null
		constraint callback_messages_pk
			primary key,
	message varchar not null,
	inline_keyboard_id int
		constraint keyboard_id_key
			references keyboard_buttons (keyboard_id)
);

create unique index callback_messages_cq_data_uindex
	on callback_messages (cq_data);

create unique index callback_messages_inline_keyboard_id_uindex
	on callback_messages (inline_keyboard_id);

create index keyboard_buttons_keyboard_id_index
	on keyboard_buttons (keyboard_id);