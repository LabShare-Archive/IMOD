/* Simple program to join two imod models together. */

#include <imodel.h>

static void stop()
{
    fprintf(stderr, "imodmkhv <in model> <out model>\n");
    fprintf(stderr, "imodmkhv: Fatal error, aborting.\n");
    exit(-1);
}

int main(int argc, char **argv)
{
    Imod *inModel;
    int ob, nob;
    
    if (argc != 3) stop();
    
    inModel = imodRead(argv[1]);
    if (!inModel) stop();

    for(ob = 0; ob < inModel->objsize; ob++){
	inModel->obj[ob].trans = 50;
    }

    if (imodOpenFile(argv[2], "w", inModel)) stop();
    imodWriteFile(inModel);
    exit(0);
}






