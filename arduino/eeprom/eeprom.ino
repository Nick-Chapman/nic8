
#define READ_EN 13
#define RESET 12
#define CLOCK 11
#define PROGRAM 10
#define DATA 2

void loop() {}
void setup() {

  Serial.begin(9600);

  pinMode(RESET, OUTPUT);
  pinMode(CLOCK, OUTPUT);
  pinMode(READ_EN, OUTPUT);
  pinMode(PROGRAM, OUTPUT);

  digitalWrite(RESET, HIGH);
  digitalWrite(CLOCK, LOW);
  digitalWrite(READ_EN, LOW);
  digitalWrite(PROGRAM, HIGH);

  //delay(500); writeStuff();
  delay(500); printContents();
}

int data[256];

int program[] = {0x4,0x1,0x2c,0x17,0x29,0x35,0x6,0x2,0xf3};

char buf[200];


void writeStuff() {

  int psize = sizeof(program)/2;
  sprintf(buf,"#program = %d...\n", psize);
  Serial.print(buf);
  for (int a = 0; a < 256; a++) data[a] = 0x00;
  for (int i = 0; i < psize; i ++) data[i] = program[i];

  digitalWrite(READ_EN, HIGH);
  delay(3);

  for (int pin = 0; pin < 8; pin++) {
    pinMode(DATA + pin, OUTPUT);
  }
  delay(3);
  sprintf(buf,"writeBytes...\n");
  Serial.print(buf);
  for (int i = 0; i < 256; i++) {
    int value = data[i];
    for (int pin = 0; pin < 8; pin++) {
      bool v = value & (1<<pin);
      digitalWrite(DATA + pin, v);
    }
    delay(3);
    digitalWrite(PROGRAM, LOW);
    delay(1);
    digitalWrite(PROGRAM, HIGH);
    delay(3);
    digitalWrite(CLOCK, HIGH);
    delay(1);
    digitalWrite(CLOCK, LOW);
    delay(3);
  }
  sprintf(buf,"writeBytes...DONE\n");
  Serial.print(buf);
}

void printContents() {
  Serial.print("--------------------\n");
  delay(1);
  digitalWrite(READ_EN, LOW);
  delay(1);
  digitalWrite(RESET, LOW);
  delay(1);
  digitalWrite(RESET, HIGH);
  delay(1);
  for (int pin = 0; pin < 8; pin++) {
    pinMode(DATA + pin, INPUT);
  }
  for (int line = 0; line < 16; line++) {
    int data[16];
    delay(3);
    for (int byte = 0; byte < 16; byte++) {
      data[byte] = 0;
      for (int bit = 0; bit < 8; bit++) {
        data[byte] += digitalRead(DATA + bit) << bit;
      }
      delay(1);
      digitalWrite(CLOCK, HIGH);
      delay(1);
      digitalWrite(CLOCK, LOW);
      delay(3);
    }
    sprintf(buf, "%3d : %02x %02x %02x %02x %02x %02x %02x %02x  "
                 "%02x %02x %02x %02x %02x %02x %02x %02x\n", 16*line,
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]
    );
    Serial.print(buf);
  }
}
