y = rnom(100)
x = rnorm(100,2)
lm.fit(y~x)  # lm() requires symbolic inputs. Here I am building a data.frame. Is this the only/best way?
df = data.frame("y" = y,
                "x" = x)
lm.fit = lm(y~x,data = df)
summary(lm.fit)
