package mrtjp.projectred.core.fx;

public final class ParticleLogicColorShift extends ParticleLogic
{
    private float minRed;
    private float maxRed;
    private float minGreen;
    private float maxGreen;
    private float minBlue;
    private float maxBlue;
    private float targetRed;
    private float targetGreen;
    private float targetBlue;
    private boolean endOnReachingColor = false;
    private float shiftSpeed;
    private float redShift;
    private float greenShift;
    private float blueShift;

    public ParticleLogicColorShift()
    {
        minRed = 0.0F;
        minGreen = 0.0F;
        minBlue = 0.0F;

        maxRed = 1.0F;
        maxGreen = 1.0F;
        maxBlue = 1.0F;

        shiftSpeed = 0.01F;

        generateNextColorTarget();
    }

    public ParticleLogicColorShift setColorTarget(float red, float green, float blue)
    {
        targetRed = red;
        targetGreen = green;
        targetBlue = blue;

        redShift = Math.abs(particle.getRed() - targetRed) * shiftSpeed;
        greenShift = Math.abs(particle.getGreen() - targetGreen) * shiftSpeed;
        blueShift = Math.abs(particle.getBlue() - targetBlue) * shiftSpeed;

        return this;
    }

    public ParticleLogicColorShift setShiftSpeed(float speed)
    {
        shiftSpeed = speed;

        redShift = shiftSpeed;
        greenShift = shiftSpeed;
        blueShift = shiftSpeed;

        return this;
    }

    public ParticleLogicColorShift setColorRange(float minRed, float minBlue, float minGreen, float maxRed, float maxGreen, float maxBlue)
    {
        this.minRed = minRed;
        this.maxRed = maxRed;
        this.minGreen = minGreen;
        this.maxGreen = maxGreen;
        this.minBlue = minBlue;
        this.maxBlue = maxBlue;
        return this;
    }

    public ParticleLogicColorShift setEndOnReachingTargetColor()
    {
        endOnReachingColor = true;
        return this;
    }

    private void generateNextColorTarget()
    {
        targetRed = rand.nextFloat() * (maxRed - minRed) + minRed;
        targetGreen = rand.nextFloat() * (maxGreen - minGreen) + minGreen;
        targetBlue = rand.nextFloat() * (maxBlue - minBlue) + minBlue;
    }

    @Override
    public void doUpdate()
    {
        float currentRed = particle.getRed();
        float currentGreen = particle.getGreen();
        float currentBlue = particle.getBlue();

        if (currentRed == targetRed && currentGreen == targetGreen && currentBlue == targetBlue)
        {
            if (endOnReachingColor)
            {
                finishLogic();
                return;
            }
            generateNextColorTarget();
        }

        particle.setRGBColorF(shiftValue(currentRed, targetRed, redShift), shiftValue(currentGreen, targetGreen, greenShift), shiftValue(currentBlue, targetBlue, blueShift));
    }

    private float shiftValue(float current, float target, float step)
    {
        float curDist = Math.abs(target - current);
        if (curDist < step)
            step = curDist;
        if (current < target)
            current += step;
        else if (current > target)
            current -= step;
        return current;
    }

    @Override
    public ParticleLogic clone()
    {
        ParticleLogicColorShift clone = new ParticleLogicColorShift().setShiftSpeed(shiftSpeed).setColorRange(minRed, minBlue, minGreen, maxRed, maxGreen, maxBlue).setColorTarget(targetRed, targetGreen, targetBlue);
        if (endOnReachingColor)
            clone.setEndOnReachingTargetColor();
        clone.setFinal(finalLogic).setPriority(priority);
        return clone;
    }
}