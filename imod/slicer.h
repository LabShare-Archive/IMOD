#define slicer_button_width  16
#define slicer_button_height 16

static unsigned char showslice_bits[] = {
     0xff, 0x0f, 0xff, 0x0f, 0xff, 0x0f, 0x00, 0x00, 0xff, 0xef, 0xff, 0xef,
     0xff, 0xe7, 0xff, 0xe9, 0xff, 0xee, 0x7f, 0xef, 0x9f, 0xef, 0xef, 0xef,
     0xf7, 0xef, 0xf9, 0xef, 0xfe, 0xef, 0xff, 0xef};

static unsigned char lock_bits[] = {
     0xf8, 0x0f, 0x0c, 0x18, 0x06, 0x30, 0x03, 0x60, 0x03, 0x60, 0x03, 0x60,
     0x03, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f, 0xff, 0x7f};

static unsigned char unlock_bits[] = {
     0xf8, 0x0f, 0x08, 0x18, 0x00, 0x30, 0x00, 0x60, 0x00, 0x60, 0x00, 0x60,
     0x00, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f, 0xff, 0x7f};

static unsigned char hide_bits[] = {
     0xff, 0xff, 0xff, 0xff, 0x03, 0x83, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00,
     0xff, 0xff, 0xff, 0xff, 0x71, 0x80, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00,
     0xff, 0xff, 0xff, 0xff, 0x01, 0xb8, 0xff, 0xff};

static unsigned char lowres_bits[] = {
     0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0x0f, 0x0f, 0x0f, 0x0f,
     0x0f, 0x0f, 0x0f, 0x0f, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0,
     0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f};

static unsigned char highres_bits[] = {
     0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33, 0xcc, 0xcc, 0xcc, 0xcc,
     0x33, 0x33, 0x33, 0x33, 0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33,
     0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33};

static unsigned char zscale_bits[] = {
     0xf8, 0x1f, 0x00, 0x10, 0x00, 0x08, 0x00, 0x04, 0x00, 0x04, 0x00, 0x02,
     0x00, 0x01, 0x00, 0x01, 0x80, 0x00, 0x80, 0x00, 0x40, 0x00, 0x20, 0x00,
     0x20, 0x00, 0x10, 0x00, 0x08, 0x00, 0xf8, 0x1f};

static unsigned char znorm_bits[] = {
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x1f, 0x00, 0x08,
     0x00, 0x06, 0x00, 0x01, 0x80, 0x00, 0x60, 0x00, 0x10, 0x00, 0xf8, 0x1f,
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
