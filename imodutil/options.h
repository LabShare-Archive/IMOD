

class bdOptionDef {
     char *label;
     char *desc;
     char **argv;
     int  args;
     int  called; // True if option, has been called.

   public:
     bdOptionDef(char *label_name, char *description);
     bdOptionDef(char *label_name, char *descriptions, int default);
     int match(char *name);
     int set(char **argv);
     virtual int getOption(void);
}

class bdOption {
     bdAList<bdOptionDef *> opts;
     int configured;
     bdOptionDef *options;
     char *program_name, *description;

   public:
     bdOption(char *pname, char *desc, int argc_start = 1);
     add(const bdOptionDef&);
     configure(int& argc, char **argv);
     bdOptionDef *get(char *label);
};



/*
int main(int argc, char **argv)
{
     bdOptionDef opd;
     bdOption    ops;
     int         opt1;

     opd.set("option1", "Takes integer arg", 0); 
     ops.add(opd);
     opt.set("option2", "Is a flag.");
     ops.add(opd);
     
     ops.configure(argc, argv);
     

}
*/
