library(devtools)

dev_mode()

load_all()

test()

document()

check()

build()
build_win()

install()

dev_mode()

system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(find.package('boris'))))