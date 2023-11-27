is_cd("$ cd jdhmlzr")
is_ls("$ ls")
is_file("248055 hlqptq.nrj")
is_dir("dir jdhmlzr")

sum(is_cd(input) | is_file(input) | is_ls(input) | is_dir(input)) == length(input)
sum(is_cd(input) & is_file(input) & is_ls(input) & is_dir(input)) == 0

extract_cd("$ cd cj") == "cj"
extract_filename("248055 hlqptq.nrj")=="hlqptq.nrj"
extract_filesize("248055 hlqptq.nrj")==248055

.cd("/a/b/", "c") == "/a/b/c/"
.cd("/a/b/c/", "..") == "/a/b/"
.cd("","/") == "/"
.cd("/a/b/","/") == "/"

names(.file("/a/b/c/", "D.txt", 55))=="/a/b/c/D.txt"
.file("/a/b/c/", "D.txt", 55) == 55

all(get_recursive_size(c("/a","/","/a/c/"),
                       disk=c("/f1"=1,
                              "/a/f2"=2,
                              "/b/f3"=3,
                              "/a/c/d/f4"=4)) == c(6,10,4))


example <- readLines("Day7/example.txt")
solve_p1(example)==95437
solve_p2(example)==24933642
