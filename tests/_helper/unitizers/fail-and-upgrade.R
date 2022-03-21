# fails with newer versions of unitizer so we can make sure upgrade shows error
# and doesn't just gag at the upgrade prompt

packageVersion('unitizer') < "1.4.15"
