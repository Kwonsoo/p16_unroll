int main(int argc, char** argv)
{
    int x = unknown();
    zoo_print (argc);
    zoo_print (argv);

    if (x > 1)
        x++;
    else 
        x--;

    argv[argc] = 1;
}
