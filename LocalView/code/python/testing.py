# expand contractions
path2 = "C:/Users/Bortimus/Desktop/text"
for fname in os.listdir(path2):
    f = os.path.join(path2, fname)
    df = open(f, 'r')
    file = df.read()
    file = contractions.fix(file)
    print(file)
    # filename = path2 + 'fixed_' + fname
    # with open(filename, "w") as f:                                                            # open file
    #     f.write(fixed)